// Copyright (c) 2022 by Oliver Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s
package validation

import scala.annotation.tailrec
import scala.runtime.BooleanRef
import scala.util.{Failure, Success, Try}

import cats.data.{Kleisli, NonEmptyChain as NEC, ValidatedNec}
import cats.implicits.*

import errors.GqlError
import errors.GqlError.*
import parsing.*
import parsing.Type.*
import parsing.Value.*

type Validated[T] = ValidatedNec[GqlError, T]

def isLeafType(`type`: Type): Boolean = `type`.name.text match
    case "Int" | "Float" | "String" | "Boolean" | "ID" => true
    case _                                             => false

def validateObjectType(`type`: Type)(using ctx: SchemaContext): Validated[Type] =
    ctx.getTypeDef(`type`.name) match
        case Some(_: ObjectTypeDefinition) => `type`.validNec
        case _                             => NonObjectType(`type`).invalidNec

def validateInputType(`type`: Type)(using ctx: SchemaContext): Validated[Type] =
    @tailrec
    def recurse(tpe: Type): Validated[Type] = tpe match
        case NonNullType(t) => recurse(t)
        case ListType(t)    => recurse(t)
        case NamedType(name) =>
            ctx.getTypeDef(name) match
                case Some(_: ScalarTypeDefinition)      => `type`.validNec
                case Some(_: EnumTypeDefinition)        => `type`.validNec
                case Some(_: InputObjectTypeDefinition) => `type`.validNec
                case _ => if isLeafType(`type`) then `type`.validNec else NonInputType(`type`).invalidNec
    recurse(`type`)

def validateOutputType(`type`: Type)(using ctx: SchemaContext): Validated[Type] =
    @tailrec
    def recurse(tpe: Type): Validated[Type] = tpe match
        case NonNullType(t) => recurse(t)
        case ListType(t)    => recurse(t)
        case NamedType(name) =>
            ctx.getTypeDef(name) match
                case Some(_: ScalarTypeDefinition)    => `type`.validNec
                case Some(_: ObjectTypeDefinition)    => `type`.validNec
                case Some(_: InterfaceTypeDefinition) => `type`.validNec
                case Some(_: UnionTypeDefinition)     => `type`.validNec
                case Some(_: EnumTypeDefinition)      => `type`.validNec
                case _ => if isLeafType(`type`) then `type`.validNec else NonOutputType(`type`).invalidNec
    recurse(`type`)

def validateUniqueName[T <: HasName](ts: List[T])(errFn: List[Name] => GqlError): Validated[List[T]] = ts
    .groupBy(_.name)
    .toList
    .traverse {
        case (_, occurance :: Nil) => occurance.validNec
        case (name, dups)          => errFn(dups.map(_.name).distinct).invalidNec
    }

def validateCovariant(a: Type, b: Type)(using ctx: SchemaContext): Validated[Type] =
    @tailrec
    def validate(aType: Type, bType: Type): Boolean =
        (aType, bType) match
            case (NonNullType(a), NonNullType(b)) => validate(a, b)
            case (NonNullType(a), b)              => validate(a, b)
            case (ListType(a), ListType(b))       => validate(a, b)
            case (ListType(a), b)                 => false
            case (a: NamedType, b: NamedType) =>
                val aTypeDef = ctx.getTypeDef(a.name)
                val bTypeDef = ctx.getTypeDef(b.name)

                (aTypeDef, bTypeDef) match
                    case (Some(a), Some(b)) if a.name == b.name => true
                    case (Some(_: ObjectTypeDefinition), Some(bUnion: UnionTypeDefinition)) =>
                        bUnion.unionMemberTypes.contains(a)
                    case (Some(aObj: ObjectLikeTypeDefinition), Some(_: InterfaceTypeDefinition)) =>
                        aObj.interfaces.contains(b)
                    case _ => false
            case (a, _) => false
    end validate

    if validate(a, b) then a.validNec
    else TypeNotCovariant(a, b).invalidNec
end validateCovariant

/**   - 5.6.4 input objects required fields
  */
def validateRequiredFields(
    objValue: ObjectValue,
    inObjTypeDef: InputObjectTypeDefinition
): Validated[List[ObjectField]] =
    inObjTypeDef.fields
        .filter(_.`type`.isInstanceOf[NonNullType])
        .traverse: inputValDef =>
            objValue.fields.find(_.name == inputValDef.name) match
                case None           => InputObjectFieldRequired(inputValDef.name, inObjTypeDef.name).invalidNec
                case Some(objField) => objField.validNec[GqlError]

/**   - 5.6.2 input object field exists
  *   - 5.6.3 input object field duplicates
  */
def validateInputObjectValue(
    inObjVal: ObjectValue,
    inObjTypeDef: InputObjectTypeDefinition
)(using ctx: SchemaContext): Validated[List[ObjectValue]] =
    @tailrec
    def recurse(
        inObjs: List[(ObjectField, InputObjectTypeDefinition)],
        acc: Validated[List[ObjectValue]]
    ): Validated[List[ObjectValue]] =
        inObjs match
            case Nil => acc

            case (ObjectField(fieldName, value: ObjectValue), inObjTypeDef) :: tail =>
                // 5.6.2 input object field exists
                inObjTypeDef.fields.find(_.name == fieldName) match
                    case None =>
                        val validations = (
                            InputObjectFieldMissing(fieldName, inObjTypeDef.name).invalidNec,

                            // 5.6.3 input object field duplicates
                            validateUniqueName(value.fields)(DuplicateFields(_))
                        ).mapN((_, _) => List(inObjVal))

                        recurse(tail, validations combine acc)

                    case Some(inValDef) =>
                        ctx.getTypeDef(inValDef.`type`.name) match
                            case Some(inObjTypeDef: InputObjectTypeDefinition) =>
                                val validations = (
                                    // 5.6.3 input object field duplicates
                                    validateUniqueName(value.fields)(DuplicateFields(_)),

                                    // 5.6.4 input objects required fields
                                    validateRequiredFields(value, inObjTypeDef)
                                ).mapN((_, _) => List(inObjVal))

                                recurse(value.fields.map(_ -> inObjTypeDef) ::: tail, validations combine acc)

                            case _ =>
                                val validations = (
                                    InputValueDefinitionMissing(inValDef.`type`.name).invalidNec,
                                    // 5.6.3 input object field duplicates
                                    validateUniqueName(value.fields)(DuplicateFields(_)),

                                    // 5.6.4 input objects required fields
                                    validateRequiredFields(value, inObjTypeDef)
                                ).mapN((_, _, _) => List(inObjVal))

                                recurse(tail, validations combine acc)

            case (ObjectField(fieldName, value), inObjTypeDef) :: tail =>
                // 5.6.2 input object field exists
                inObjTypeDef.fields.find(_.name == fieldName) match
                    case None =>
                        val validatedMissingField = InputObjectFieldMissing(fieldName, inObjTypeDef.name).invalidNec
                        recurse(tail, validatedMissingField combine acc)
                    case Some(_) => recurse(tail, acc)
    end recurse

    val validations = (
        validateUniqueName(inObjVal.fields)(DuplicateFields(_)), // 5.6.3 input object field duplicates
        validateRequiredFields(inObjVal, inObjTypeDef)           // 5.6.4 input objects required fields
    ).mapN((_, _) => List(inObjVal))

    recurse(inObjVal.fields.map(_ -> inObjTypeDef), validations)
end validateInputObjectValue

def validateValue(value: Value, valueType: Type)(validateVariable: (Variable, Type) => Validated[Value])(using
    ctx: SchemaContext
): Validated[Value] =
    valueType match
        case ListType(listType) =>
            value match
                case ListValue(values) => values.traverse(validateValue(_, listType)(validateVariable)).map(_ => value)
                case v: Variable       => validateVariable(v, valueType)
                case _                 => TypeMismatch(value, valueType).invalidNec

        case NonNullType(requiredType) =>
            value match
                case NullValue   => NullValueFound(valueType.name).invalidNec
                case v: Variable => validateVariable(v, valueType)
                case _           => validateValue(value, requiredType)(validateVariable)

        case NamedType(name) =>
            val typeDef = ctx.getTypeDef(name).get
            typeDef match
                case ScalarTypeDefinition(Name("Int"), _) =>
                    value match
                        case v @ IntValue(value) =>
                            Try(value.toInt) match
                                case Success(_) => v.valid
                                case Failure(_) => TypeMismatch(v, NamedType(Name("Int"))).invalidNec
                        case v @ Variable(_) => validateVariable(v, NamedType(Name("Int")))
                        case v @ NullValue   => v.valid
                        case v               => TypeMismatch(v, NamedType(Name("Int"))).invalidNec
                case ScalarTypeDefinition(Name("Float"), _) =>
                    value match
                        case v @ FloatValue(value) =>
                            Try(value.toFloat) match
                                case Success(_) => v.valid
                                case Failure(_) => TypeMismatch(v, NamedType(Name("Float"))).invalidNec
                        case v @ Variable(_) => validateVariable(v, NamedType(Name("Float")))
                        case v @ NullValue   => v.valid
                        case v               => TypeMismatch(v, NamedType(Name("Float"))).invalidNec
                case ScalarTypeDefinition(Name("String"), _) =>
                    value match
                        case v @ StringValue(_) => v.valid
                        case v @ Variable(_)    => validateVariable(v, NamedType(Name("String")))
                        case v @ NullValue      => v.valid
                        case v                  => TypeMismatch(v, NamedType(Name("String"))).invalidNec
                case ScalarTypeDefinition(Name("Boolean"), _) =>
                    value match
                        case v @ BooleanValue(value) =>
                            Try(value.toBoolean) match
                                case Success(_) => v.valid
                                case Failure(_) => TypeMismatch(v, NamedType(Name("Boolean"))).invalidNec
                        case v @ Variable(_) => validateVariable(v, NamedType(Name("Boolean")))
                        case v @ NullValue   => v.valid
                        case v               => TypeMismatch(v, NamedType(Name("Boolean"))).invalidNec
                case ScalarTypeDefinition(Name("ID"), _) =>
                    value match
                        case v @ StringValue(value) => v.valid
                        case v @ IntValue(value) =>
                            Try(value.toInt) match
                                case Success(_) => v.valid
                                case Failure(_) => TypeMismatch(v, NamedType(Name("ID"))).invalidNec
                        case v @ Variable(_) => validateVariable(v, NamedType(Name("ID")))
                        case v @ NullValue   => v.valid
                        case v               => TypeMismatch(v, NamedType(Name("ID"))).invalidNec

                case x: ScalarTypeDefinition =>
                    // TODO: Not sure what to do here!
                    ???

                case typeDef: EnumTypeDefinition =>
                    value match
                        case value: EnumValue if typeDef.values.exists(_.value == value) => value.valid
                        case v @ Variable(_) => validateVariable(v, valueType)
                        case NullValue       => value.valid
                        case _               => TypeMismatch(value, valueType).invalidNec

                case typeDef: InputObjectTypeDefinition =>
                    value match
                        case value @ ObjectValue(fields) =>
                            validateInputObjectValue(value, typeDef)
                                .andThen(_ =>
                                    fields.traverse(field =>
                                        val expectedType = typeDef.fields.find(_.name == field.name).get.`type`
                                        validateValue(field.value, expectedType)(validateVariable)
                                    )
                                )
                                .map(_ => value)
                        case v @ Variable(_) => validateVariable(v, valueType)
                        case NullValue       => value.valid
                        case _               => TypeMismatch(value, valueType).invalidNec

                // never valid inputs
                case _: (ObjectTypeDefinition | InterfaceTypeDefinition | UnionTypeDefinition) =>
                    IllegalInputType(valueType).invalidNec
end validateValue

/** Validate a fields arguments
  * @param args
  *   The arguments we will validate.
  * @param def
  *   The definition the arguments belong to (either field or directive definition)
  * @param parentType
  *   The type of the object containing the field
  */
def validateArgs(
    args: List[Argument],
    `def`: FieldDefinition | DirectiveDefinition
)(
    validateVariable: (Variable, Type) => Validated[Value]
)(using ctx: SchemaContext): Validated[List[Argument]] =
    val validatedArgs =
        args.traverse { 
            case arg @ Argument(name, _) =>
                // 5.4.1 args exist
                `def`.arguments.find(_.name == name) match
                    case None         => InputValueDefinitionMissing(name).invalidNec
                    case Some(argDef) => (arg -> argDef).validNec
        }.andThen: argsWithDef =>
            argsWithDef.traverse { 
                case (arg, argDef) => validateValue(arg.value, argDef.`type`)(validateVariable) 
            }

    // 5.4.2 args are unique
    val validatedArgNames = validateUniqueName(args)(DuplicateArguments(_))

    // 5.4.2.1 required args
    val requiredArgs =
        `def`.arguments
            .filter {
                case InputValueDefinition(_, _: NonNullType, None, _) => true
                case _                                                => false
            }
            .map(_.name)
    val validatedRequiredArgs = requiredArgs.traverse(argName =>
        args.find(_.name == argName) match
            case None    => RequiredArgumentMissing(argName, `def`.name).invalidNec
            case Some(_) => ().validNec
    )

    (
        validatedArgs,
        validatedArgNames,
        validatedRequiredArgs
    ).mapN((_, _, _) => args)
end validateArgs

/**   - 5.7.1 Directive definitions should exist.
  *   - 5.7.2 Directive must be used in a valid location
  */
def validateDirective(dir: Directive, currLoc: DirectiveLocation)(using
    ctx: SchemaContext
): Validated[(Directive, DirectiveDefinition)] =
    val validateDirDefExists = ctx.getDirectiveDef(dir.name) match
        case Some(dirDef) => dirDef.validNec
        case None         => DirectiveDefinitionMissing(dir.name).invalidNec

    validateDirDefExists
        .andThen: dirDef =>
            val validatedArgs = validateArgs(dir.arguments, dirDef)((value, _) => value.validNec)
            val validatedLocation =
                if dirDef.directiveLocs.find(_ == currLoc).isDefined then dirDef.validNec
                else IllegalDirectiveLocation(dir.name, currLoc).invalidNec

            (
                validatedArgs,
                validatedLocation
            ).mapN((_, _) => dir -> dirDef)

/**   - 5.7.1 Directive definitions should exist.
  *   - 5.7.2 Directive must be used in a valid location
  *   - 5.7.3
  */
def validateDirectives(dirs: List[Directive], currLoc: DirectiveLocation)(using
    ctx: SchemaContext
): Validated[List[Directive]] =
    dirs
        .map(validateDirective(_, currLoc).map(List(_)))
        .reduceOption(_ combine _)
        .getOrElse(Nil.validNec)
        .andThen: dirsWithDef =>
            dirsWithDef
                .filterNot(_._2.repeatable)
                .groupBy(_._1.name)
                .toList
                .traverse {
                    case (name, (dir, _) :: Nil) => dir.validNec
                    case (name, duplicates) =>
                        DuplicateDirectives(duplicates.map { case (dir, _) => dir.name }.distinct).invalidNec
                }
            // .map { case (_, occurences) =>
            //     occurences(0)._1 -> occurences.length
            // }
            // .map {
            //     case (dir, count) if count == 1 => List(dir).validNec
            //     case (dir, count)               => DuplicateName(dir.name).invalidNec
            // }
            // .reduceOption(_ combine _)
            // .getOrElse(Nil.validNec)
end validateDirectives

// TODO:
// ✔️✔️ 3.13 (why isn't this in section 5?!?) directive definitions need to be validated
// 5.3.2 Field merging needs to be implemented (BIG)
// ✔️✔️ 5.4.1, 5.4.2 & 5.4.2.1 must also be applied to directives
// 5.5.2.3.4 Abstract spreads within abstract scope need to be validated
// ✔️✔️ 5.6.1 Value type must be validated
// 5.8.5 Variable usages must be valid (BIG)

// Extensions need to be validated (LATER)

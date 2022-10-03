// Copyright (c) 2022 by Oliver Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s
package validation

import scala.annotation.tailrec
import scala.runtime.BooleanRef
import scala.util.{Failure, Success, Try}

import cats.data.Kleisli
import cats.data.ValidatedNec
import cats.implicits.*

import errors.GqlError
import errors.GqlError.*
import parsing.*
import parsing.Type.*
import parsing.Value.*

type Validated[T] = ValidatedNec[GqlError, T]

def isLeafType(name: Name): Boolean = name.text match
    case "Int" | "Float" | "String" | "Boolean" | "ID" => true
    case _                                             => false

def isObjectType(name: Name)(using ctx: SchemaContext): Boolean = ctx.getTypeDef(name) match
    case Some(_: ObjectTypeDefinition) => true
    case _                             => false

@tailrec
def isInputType(`type`: Type)(using ctx: SchemaContext): Boolean = `type` match
    case NonNullType(tpe) => isInputType(tpe)
    case ListType(tpe)    => isInputType(tpe)
    case NamedType(name) =>
        ctx.getTypeDef(name) match
            case Some(_: ScalarTypeDefinition)      => true
            case Some(_: EnumTypeDefinition)        => true
            case Some(_: InputObjectTypeDefinition) => true
            case _                                  => isLeafType(name)

@tailrec
def isOutputType(`type`: Type)(using ctx: SchemaContext): Boolean = `type` match
    case NonNullType(tpe) => isOutputType(tpe)
    case ListType(tpe)    => isOutputType(tpe)
    case NamedType(name) =>
        ctx.getTypeDef(name) match
            case Some(_: ScalarTypeDefinition)    => true
            case Some(_: ObjectTypeDefinition)    => true
            case Some(_: InterfaceTypeDefinition) => true
            case Some(_: UnionTypeDefinition)     => true
            case Some(_: EnumTypeDefinition)      => true
            case _                                => isLeafType(name)

def validateUniqueName[T <: HasName](ts: List[T]): Validated[List[T]] = ts
    .groupBy(_.name)
    .toList
    .traverse {
        case (_, occurance :: Nil) => occurance.validNec
        case (name, _)             => DuplicateName(name).invalidNec
    }

def validateIsUsed(definitions: List[Name], references: List[Name]): Validated[List[Name]] =
    definitions
        .traverse(definition =>
            if references.contains(definition) then definition.validNec
            else UnusedDefinition(definition).invalidNec
        )

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
    else InvalidType(a, Some(s"$a is not covariant with $b")).invalidNec
end validateCovariant

/**   - 5.6.4 input objects required fields
  */
def validateRequiredFields(
    objValue: ObjectValue,
    inObjTypeDef: InputObjectTypeDefinition
): Validated[List[ObjectField]] =
    inObjTypeDef.fields
        .filter(_.`type`.isInstanceOf[NonNullType])
        .traverse(inputValDef =>
            objValue.fields.find(_.name == inputValDef.name) match
                case None           => MissingField2(inputValDef.name, NamedType(inObjTypeDef.name)).invalidNec
                case Some(objField) => objField.validNec[GqlError]
        )

/**   - 5.6.2 input object field exists
  *   - 5.6.3 input object field duplicates
  */
def validateInputObjectValue(
    inObjVal: ObjectValue,
    // inValDef: InputValueDefinition,
    inObjTypeDef: InputObjectTypeDefinition
)(using ctx: SchemaContext): Validated[List[ObjectValue]] =
    @tailrec
    def recurse(
        inObjs: List[(ObjectField, InputObjectTypeDefinition)],
        acc: Validated[List[ObjectValue]]
    ): Validated[List[ObjectValue]] =
        inObjs match
            case Nil => acc

            case (ObjectField(name, value: ObjectValue), inObjTypeDef) :: tail =>
                // 5.6.2 input object field exists
                inObjTypeDef.fields.find(_.name == name) match
                    case None =>
                        val parentType: NamedType = NamedType(inObjTypeDef.name)
                        val validations = (
                            MissingField2(name, parentType).invalidNec,
                            validateUniqueName(value.fields) // 5.6.3 input object field duplicates
                        ).mapN((_, _) => List(inObjVal))

                        recurse(tail, validations combine acc)

                    case Some(inValDef) =>
                        ctx.getTypeDef(inValDef.`type`.name) match
                            case Some(inObjTypeDef: InputObjectTypeDefinition) =>
                                val validations = (
                                    validateUniqueName(value.fields),           // 5.6.3 input object field duplicates
                                    validateRequiredFields(value, inObjTypeDef) // 5.6.4 input objects required fields
                                ).mapN((_, _) => List(inObjVal))

                                recurse(value.fields.map(_ -> inObjTypeDef) ::: tail, validations combine acc)

                            case _ =>
                                val validations = (
                                    MissingDefinition(inValDef.`type`.name).invalidNec,
                                    validateUniqueName(value.fields),           // 5.6.3 input object field duplicates
                                    validateRequiredFields(value, inObjTypeDef) // 5.6.4 input objects required fields
                                ).mapN((_, _, _) => List(inObjVal))

                                recurse(tail, validations combine acc)

            case (ObjectField(name, value), inObjTypeDef) :: tail =>
                // 5.6.2 input object field exists
                inObjTypeDef.fields.find(_.name == name) match
                    case None =>
                        val validatedMissingField = MissingField2(name, NamedType(inObjTypeDef.name)).invalidNec
                        recurse(tail, validatedMissingField combine acc)
                    case Some(_) => recurse(tail, acc)
    end recurse

    // ctx.getTypeDef(inValDef.`type`.name) match
    //     case Some(inObjTypeDef: InputObjectTypeDefinition) =>
    //         val validations = (
    //             validateUniqueName(inObjVal.fields),           // 5.6.3 input object field duplicates
    //             validateRequiredFields(inObjVal, inObjTypeDef) // 5.6.4 input objects required fields
    //         ).mapN((_, _) => List(inObjVal))

    //         recurse(inObjVal.fields.map(_ -> inObjTypeDef), validations)

    //     case _ =>
    //         (
    //             MissingDefinition(inValDef.`type`.name).invalidNec,
    //             validateUniqueName(inObjVal.fields) // 5.6.3 input object field duplicates
    //         ).mapN((_, _) => List(inObjVal))

    val validations = (
        validateUniqueName(inObjVal.fields),           // 5.6.3 input object field duplicates
        validateRequiredFields(inObjVal, inObjTypeDef) // 5.6.4 input objects required fields
    ).mapN((_, _) => List(inObjVal))

    recurse(inObjVal.fields.map(_ -> inObjTypeDef), validations)
end validateInputObjectValue

def validateVar(variable: Variable, expectedTypeName: Name): Validated[Value] =
    MissingDefinition(variable.name).invalidNec

def validateArgValues(
    args: List[Argument],
    within: FieldDefinition | DirectiveDefinition,
    validateVar: (variable: Variable, expectedTypeName: Name) => Validated[Value] = validateVar
)(using ctx: SchemaContext): Validated[List[Value]] =
    def validateValue(value: Value, valueType: Type): Validated[Value] =
        valueType match
            case ListType(listType) =>
                value match
                    case ListValue(values) =>
                        values.traverse(validateValue(_, listType)).map(_ => value)
                    case _ => TypeMismatch2(value, listType.name).invalidNec

            case NonNullType(requiredType) =>
                value match
                    case NullValue => NullValueFound(valueType.name).invalidNec
                    case _         => validateValue(value, requiredType)

            case NamedType(name) =>
                val typeDef = ctx.getTypeDef(name).get
                typeDef match
                    case ScalarTypeDefinition(Name("Int"), _) =>
                        value match
                            case v @ IntValue(value) =>
                                Try(value.toInt) match
                                    case Success(_) => v.valid
                                    case Failure(_) => TypeMismatch2(v, Name("Int")).invalidNec
                            case v @ Variable(_) => validateVar(v, Name("Int"))
                            case v @ NullValue   => v.valid
                            case v               => TypeMismatch2(v, Name("Int")).invalidNec
                    case ScalarTypeDefinition(Name("Float"), _) =>
                        value match
                            case v @ FloatValue(value) =>
                                Try(value.toFloat) match
                                    case Success(_) => v.valid
                                    case Failure(_) => TypeMismatch2(v, Name("Float")).invalidNec
                            case v @ Variable(_) => validateVar(v, Name("Float"))
                            case v @ NullValue   => v.valid
                            case v               => TypeMismatch2(v, Name("Float")).invalidNec
                    case ScalarTypeDefinition(Name("String"), _) =>
                        value match
                            case v @ StringValue(_) => v.valid
                            case v @ Variable(_)    => validateVar(v, Name("String"))
                            case v @ NullValue      => v.valid
                            case v                  => TypeMismatch2(v, Name("String")).invalidNec
                    case ScalarTypeDefinition(Name("Boolean"), _) =>
                        value match
                            case v @ BooleanValue(value) =>
                                Try(value.toBoolean) match
                                    case Success(_) => v.valid
                                    case Failure(_) => TypeMismatch2(v, Name("Boolean")).invalidNec
                            case v @ Variable(_) => validateVar(v, Name("Boolean"))
                            case v @ NullValue   => v.valid
                            case v               => TypeMismatch2(v, Name("Boolean")).invalidNec
                    case ScalarTypeDefinition(Name("ID"), _) =>
                        value match
                            case v @ StringValue(value) => v.valid
                            case v @ IntValue(value) =>
                                Try(value.toInt) match
                                    case Success(_) => v.valid
                                    case Failure(_) => TypeMismatch2(v, Name("ID")).invalidNec
                            case v @ Variable(_) => validateVar(v, Name("ID"))
                            case v @ NullValue   => v.valid
                            case v               => TypeMismatch2(v, Name("ID")).invalidNec

                    case x: ScalarTypeDefinition =>
                        // TODO: Not sure what to do here!
                        ???

                    case typeDef: EnumTypeDefinition =>
                        value match
                            case value: EnumValue if typeDef.values.exists(_.value == value) => value.valid
                            case value @ Variable(_) => validateVar(value, typeDef.name)
                            case NullValue           => value.valid
                            case _                   => TypeMismatch2(value, typeDef.name).invalidNec

                    case typeDef: InputObjectTypeDefinition =>
                        value match
                            case value @ ObjectValue(fields) =>
                                validateInputObjectValue(value, typeDef)
                                    .andThen(_ =>
                                        fields.traverse(field =>
                                            val expectedType = typeDef.fields.find(_.name == field.name).get.`type`
                                            validateValue(field.value, expectedType)
                                        )
                                    )
                                    .map(_ => value)
                            case value @ Variable(_) => validateVar(value, typeDef.name)
                            case NullValue           => value.valid
                            case _                   => TypeMismatch2(value, typeDef.name).invalidNec

                    // never valid inputs
                    case _: (ObjectTypeDefinition | InterfaceTypeDefinition | UnionTypeDefinition) =>
                        InvalidType(valueType).invalidNec
    end validateValue

    args.traverse(arg =>
        val argDef = within.arguments.find(_.name == arg.name).get // Must exist if we've got here
        validateValue(arg.value, argDef.`type`)
    )
end validateArgValues

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
)(using ctx: SchemaContext): Validated[List[Argument]] =
    val validatedArgs =
        args.map { case Argument(name, _) =>
            // 5.4.1 args exist
            `def`.arguments.find(_.name == name) match
                case None =>
                    MissingDefinition(
                        name,
                        Some(s"in definition ${`def`.name}")
                    ).invalidNec
                case _ => ().validNec
        }.combineAll

    // 5.4.2 args are unique
    val validatedArgNames = validateUniqueName(args).map(_ => ())

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
            case None    => MissingArgument2(argName).invalidNec
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
        case None         => MissingDefinition(dir.name).invalidNec

    validateDirDefExists
        .andThen(dirDef =>
            val validatedArgs = validateArgs(dir.arguments, dirDef)
            // val validatedArgValues = validateArgValues(dir.arguments, dirDef)
            val validatedLocation =
                if dirDef.directiveLocs.find(_ == currLoc).isDefined then dirDef.validNec
                else InvalidLocation(dir.name).invalidNec

            (
                validatedArgs,
                // validatedArgValues,
                validatedLocation
            ).mapN((_, _) => dir -> dirDef)
        )
end validateDirective

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
        .andThen(dirsWithDef =>
            dirsWithDef
                .filterNot(_._2.repeatable)
                .groupBy(_._1.name)
                .toList
                .map { case (_, occurences) =>
                    occurences(0)._1 -> occurences.length
                }
                .map {
                    case (dir, count) if count == 1 => List(dir).validNec
                    case (dir, count)               => DuplicateName(dir.name).invalidNec
                }
                .reduceOption(_ combine _)
                .getOrElse(Nil.validNec)
        )
end validateDirectives

// TODO:
// ✔️✔️ 3.13 (why isn't this in section 5?!?) directive definitions need to be validated
// 5.3.2 Field merging needs to be implemented (BIG)
// ✔️✔️ 5.4.1, 5.4.2 & 5.4.2.1 must also be applied to directives
// 5.5.2.3.4 Abstract spreads within abstract scope need to be validated
// 5.6.1 Value type must be validated
// 5.8.5 Variable usages must be valid (BIG)

// Extensions need to be validated (LATER)

// Copyright (c) 2022 by Oliver Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s
package validation

import scala.annotation.tailrec
import cats.data.ValidatedNec
import cats.implicits.*
import errors.GqlError
import parsing.*

import GqlError.*
import Value.*
import Type.*

type Validated[T] = ValidatedNec[GqlError, T]

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

/**
 *   - 5.6.2 input object field exists
 *   - 5.6.3 input object field duplicates
 *   - 5.6.4 input objects required fields
 */
def validateInputObjectValue(
    inObjVal: ObjectValue,
    inValDef: InputValueDefinition
)(using schema: TypeSystemDocument): Validated[ObjectValue] =
  @tailrec
  def recurse(
      inObjs: List[(ObjectField, InputObjectTypeDefinition)],
      acc: Validated[Unit]
  ): Validated[Unit] =
    inObjs match
      case Nil => acc

      case (ObjectField(name, value: ObjectValue), inObjTypeDef) :: tail =>
        // 5.6.3 input object field duplicates
        val validatedFieldNames = validateUniqueName(value.fields).map(_ => ())

        // 5.6.2 input object field exists
        inObjTypeDef.fields.find(_.name == name) match
          case None =>
            val parentType: NamedType = NamedType(inObjTypeDef.name)
            val validatedMissingField = MissingField2(name, parentType).invalidNec
            recurse(tail, validatedFieldNames combine validatedMissingField combine acc)
          case Some(inValDef) =>
            schema.findTypeDef[InputObjectTypeDefinition](inValDef.`type`.name) match
              case None =>
                val validatedMissingDef = MissingDefinition(inValDef.`type`.name).invalidNec
                recurse(tail, validatedFieldNames combine validatedMissingDef combine acc)
              case Some(inObjTypeDef) =>
                // 5.6.4 input objects required fields
                val validatedRequiredFields = inObjTypeDef.fields
                  .filter(_.`type`.isInstanceOf[NonNullType])
                  .traverse(inputValDef =>
                    value.fields.find(_.name == inputValDef.name) match
                      case None =>
                        MissingField2(inputValDef.name, NamedType(inObjTypeDef.name))
                          .invalidNec[Unit]
                      case Some(_) => ().validNec[GqlError]
                  )
                  .map(_ => ())

                recurse(
                  value.fields.map(_ -> inObjTypeDef) ::: tail,
                  validatedFieldNames combine validatedRequiredFields combine acc
                )

      case (ObjectField(name, value), inObjTypeDef) :: tail =>
        // 5.6.2 input object field exists
        inObjTypeDef.fields.find(_.name == name) match
          case None =>
            val validatedMissingField =
              MissingField2(name, NamedType(inObjTypeDef.name)).invalidNec
            recurse(tail, validatedMissingField combine acc)
          case Some(_) => recurse(tail, acc)
  end recurse

  // 5.6.3 input object field duplicates
  val validatedFieldNames = validateUniqueName(inObjVal.fields).map(_ => ())

  val validatedInObjVal =
    schema.findTypeDef[InputObjectTypeDefinition](inValDef.`type`.name) match
      case None =>
        val validatedTypeDef = MissingDefinition(inValDef.`type`.name).invalidNec[Unit]
        validatedFieldNames combine validatedTypeDef
      case Some(inObjTypeDef) =>
        // 5.6.4 input objects required fields
        val validatedRequiredFields = inObjTypeDef.fields
          .filter(_.`type`.isInstanceOf[NonNullType])
          .map(_.name)
          .map(name =>
            inObjVal.fields.find(_.name == name) match
              case None    => MissingField2(name, NamedType(inObjTypeDef.name)).invalidNec
              case Some(_) => ().validNec
          )
          .reduceLeft(_ combine _)

        recurse(
          inObjVal.fields.map(_ -> inObjTypeDef),
          validatedFieldNames combine validatedRequiredFields
        )

  validatedInObjVal.map(_ => inObjVal)
end validateInputObjectValue

/**
 * Validate a fields arguments
 * @param args
 *   The arguments we will validate.
 * @param def
 *   The definition the arguments belong to (either field or directive definition)
 * @param parentType
 *   The type of the object containing the field
 */
def validateArguments(args: List[Argument], `def`: HasName & HasArgs)(using
    schema: TypeSystemDocument
): Validated[List[Argument]] =
  val validatedArgs =
    args.map {
      case Argument(name, objVal: ObjectValue) =>
        // 5.4.1 args exist
        `def`.arguments.find(_.name == name) match
          case None =>
            MissingDefinition(
              name,
              Some(s"in definition ${`def`.name}")
            ).invalidNec
          case Some(inValDef) =>
            validateInputObjectValue(objVal, inValDef).map(_ => ())

      case Argument(name, _) =>
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
end validateArguments

/**
 *   - 5.7.1 Directive definitions should exist.
 *   - 5.7.2 Directive must be used in a valid location
 */
def validateDirective(dir: Directive, currLoc: DirectiveLocation)(using
    schema: TypeSystemDocument
): Validated[(Directive, DirectiveDefinition)] =
  val validateDirDefExists = schema.findDirectiveDef(dir.name) match
    case Some(dirDef) => dirDef.validNec
    case None         => MissingDefinition(dir.name).invalidNec

  validateDirDefExists
    .andThen(dirDef =>
      val validatedArgs = validateArguments(dir.arguments, dirDef)
      val validatedLocation =
        if dirDef.directiveLocs.find(_ == currLoc).isDefined then dirDef.validNec
        else InvalidLocation(dir.name).invalidNec

      (
        validatedArgs,
        validatedLocation
      ).mapN((_, _) => dir -> dirDef)
    )
end validateDirective

/**
 *   - 5.7.1 Directive definitions should exist.
 *   - 5.7.2 Directive must be used in a valid location
 *   - 5.7.3
 */
def validateDirectives(dirs: List[Directive], currLoc: DirectiveLocation)(using
    schema: TypeSystemDocument
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

// ✔️ 3.13 (why isn't this in section 5?!?) directive definitions need to be validated
// 5.3.2 Field merging needs to be implemented (BIG)
// ✔️ 5.4.1, 5.4.2 & 5.4.2.1 must also be applied to directives
// 5.5.2.3.4 Abstract spreads within abstract scope need to be validated
// 5.6.1 Value type must be validated
// 5.8.5 Variable usages must be valid (BIG)

// Extensions need to be validated (LATER)

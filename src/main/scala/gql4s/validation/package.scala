// Copyright (c) 2022 by Oliver Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s
package validation

import cats.data.ValidatedNec
import cats.implicits.*
import errors.GqlError
import parsing.*

import GqlError.*

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
      if dirDef.directiveLocs.find(_ == currLoc).isDefined then dirDef.validNec
      else InvalidLocation(dir.name).invalidNec
    )
    .map(dirDef => dir -> dirDef)
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

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

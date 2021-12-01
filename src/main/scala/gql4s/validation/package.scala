// Copyright (c) 2018-2021 by Oli Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s
package validation

import cats.implicits.*
import gql4s.parsers.*

// 5.2.1.1 (10-2021)
def operationNameUniqueness(doc: ExecutableDocument): Either[String, Unit] =
  val operations       = doc.filter(_.isInstanceOf[OperationDefinition])
  val uniqueOperations = operations.distinctBy(_.name)
  if uniqueOperations.length == operations.length then ().asRight else "Fuck".asLeft

// Copyright (c) 2018-2021 by Oli Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s
package validation

import cats.data.NonEmptyList
import parsers.*
import munit.FunSuite

class ValidationSuite extends FunSuite:
  test("operations") {
    val doc1Str = """
      query getDogName {
        dog {
          name
        }
      }

      query getOwnerName {
        dog {
          owner {
            name
          }
        }
      }
    """
    executableDocument.parse(doc1Str) match {
      case Right(_ -> doc) => assert(operationNameUniqueness(doc).isRight)
      case _               => fail("failed to parse doc1")
    }

    val doc2Str = """
      query dogOperation {
        dog {
          name
        }
      }

      mutation dogOperation {
        mutateDog {
          id
        }
      }
    """
    executableDocument.parse(doc2Str) match {
      case Right(_ -> doc) => assert(operationNameUniqueness(doc).isLeft)
      case _               => fail("failed to parse doc2")
    }
  }

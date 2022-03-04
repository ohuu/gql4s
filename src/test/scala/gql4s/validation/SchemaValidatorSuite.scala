// Copyright (c) 2021 by Oliver Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s
package validation

import munit.FunSuite
import SchemaValidator.*
import Type.*

class SchemaValidatorSuite extends FunSuite:
  test("Any type is a subtype of itself") {
    val A    = schema2.findInterfaceTypeDef(NamedType(Name("A"))).get
    val errs = isValidImplementation(A, A, schema2)
    assertEquals(clue(errs), Nil)
  }
end SchemaValidatorSuite

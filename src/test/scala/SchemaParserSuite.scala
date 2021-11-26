// Copyright (c) 2018-2021 by Oli Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package parsers

import cats.data.NonEmptyList
import munit.FunSuite
import Type.*

class SchemaParserSuite extends FunSuite:
  test("scalars") {}

  test("objects") {}

  test("interfaces") {}

  test("unions") {
    val test1    = "= Dog | Fish"
    val test1Res = NonEmptyList(NamedType(Name("Dog")), List(NamedType(Name("Fish"))))
    assert(clue(unionMemberType.parse(test1)) == Right("", test1Res))
  }

  test("enums") {}

  test("input objects") {}

  test("directives") {}

  test("schema") {}

  test("type system") {}

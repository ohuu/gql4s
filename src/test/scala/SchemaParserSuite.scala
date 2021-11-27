// Copyright (c) 2018-2021 by Oli Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package parsers

import cats.data.NonEmptyList
import munit.FunSuite
import Type.*
import Value.*

class SchemaParserSuite extends FunSuite:
  test("scalars") {
    val test1 = "\"\"\"hello\"\"\"scalar UUID"
    val res1  = ScalarTypeDefinition(Name("UUID"), Nil)

    val test2 = s"""${"""""""""}
    hello
    ${"""""""""}
    scalar UUID @specifiedBy(url: "https://tools.ietf.org/html/rfc4122")
    """
    val res2 = ScalarTypeDefinition(
      Name("UUID"),
      List(
        Directive(
          Name("specifiedBy"),
          List(Argument(Name("url"), StringValue("https://tools.ietf.org/html/rfc4122")))
        )
      )
    )

    assert(clue(scalarTypeDefinition.parse(test1)) == Right("", res1))
    assert(clue(scalarTypeDefinition.parse(test2)) == Right("", res2))
  }

  test("objects") {}

  test("interfaces") {}

  test("unions") {
    // val test1    = "= Dog | Fish"
    // val test1Res = NonEmptyList(NamedType(Name("Dog")), List(NamedType(Name("Fish"))))
    // assert(clue(unionMemberType.parse(test1)) == Right("", test1Res))
  }

  test("enums") {}

  test("input objects") {}

  test("directives") {}

  test("schema") {}

  test("type system") {}

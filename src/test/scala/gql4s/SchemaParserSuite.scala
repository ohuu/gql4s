// Copyright (c) 2021 by Oliver Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s

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

    val test3 = "extend scalar Int @skip"
    val res3  = ScalarTypeExtension(Name("Int"), NonEmptyList.one(Directive(Name("skip"), Nil)))

    assert(clue(scalarTypeDefinition.parse(test1)) == Right("", res1))
    assert(clue(scalarTypeDefinition.parse(test2)) == Right("", res2))
    assert(clue(scalarTypeExtension.parse(test3)) == Right("", res3))
  }

  test("objects") {
    val test1 = """type Person {
      name: String
      age: Int!
      picture(size: Int!): Url
    }"""
    val res1 = ObjectTypeDefinition(
      Name("Person"),
      Nil,
      Nil,
      List(
        FieldDefinition(Name("name"), Nil, NamedType(Name("String")), Nil),
        FieldDefinition(Name("age"), Nil, NonNullType(NamedType(Name("Int"))), Nil),
        FieldDefinition(
          Name("picture"),
          List(InputValueDefinition(Name("size"), NonNullType(NamedType(Name("Int"))), None, Nil)),
          NamedType(Name("Url")),
          Nil
        )
      )
    )

    val test2 = """type Business implements NamedEntity & ValuedEntity {
      name: String
      value: Int
      employeeCount: Int
    }"""
    val res2 = ObjectTypeDefinition(
      Name("Business"),
      List(NamedType(Name("NamedEntity")), NamedType(Name("ValuedEntity"))),
      Nil,
      List(
        FieldDefinition(Name("name"), Nil, NamedType(Name("String")), Nil),
        FieldDefinition(Name("value"), Nil, NamedType(Name("Int")), Nil),
        FieldDefinition(Name("employeeCount"), Nil, NamedType(Name("Int")), Nil)
      )
    )

    val test3 = """extend type Address implements HasPostcode {
      postcode: String!
    }"""
    val res3 = ObjectTypeExtension(
      Name("Address"),
      List(NamedType(Name("HasPostcode"))),
      Nil,
      List(FieldDefinition(Name("postcode"), Nil, NonNullType(NamedType(Name("String"))), Nil))
    )

    assert(clue(objectTypeDefinition.parse(test1)) == Right("", res1))
    assert(clue(objectTypeDefinition.parse(test2)) == Right("", res2))
    assert(clue(objectTypeExtension.parse(test3)) == Right("", res3))
  }

  test("interfaces") {
    val test1 = """interface NamedEntity {
      name: String
    }"""
    val res1 = InterfaceTypeDefinition(
      Name("NamedEntity"),
      Nil,
      Nil,
      List(FieldDefinition(Name("name"), Nil, NamedType(Name("String")), Nil))
    )

    val test2 = """interface Resource implements Node {
      id: ID!
      url: String
    }"""
    val res2 = InterfaceTypeDefinition(
      Name("Resource"),
      List(NamedType(Name("Node"))),
      Nil,
      List(
        FieldDefinition(Name("id"), Nil, NonNullType(NamedType(Name("ID"))), Nil),
        FieldDefinition(Name("url"), Nil, NamedType(Name("String")), Nil)
      )
    )

    val test3 = """extend interface NamedEntity {
      nickname: String
    }"""
    val res3 = InterfaceTypeExtension(
      Name("NamedEntity"),
      Nil,
      Nil,
      List(FieldDefinition(Name("nickname"), Nil, NamedType(Name("String")), Nil))
    )

    assert(clue(interfaceTypeDefinition.parse(test1)) == Right("", res1))
    assert(clue(interfaceTypeDefinition.parse(test2)) == Right("", res2))
    assert(clue(interfaceTypeExtension.parse(test3)) == Right("", res3))
  }

  test("unions") {
    val test1 = "union DogOrFish = Dog | Fish"
    val test1Res = UnionTypeDefinition(
      Name("DogOrFish"),
      Nil,
      List(NamedType(Name("Dog")), NamedType(Name("Fish")))
    )

    val test2 = """union SearchResult =
      | Photo
      | Person"""
    val test2Res = UnionTypeDefinition(
      Name("SearchResult"),
      Nil,
      List(NamedType(Name("Photo")), NamedType(Name("Person")))
    )

    val test3    = """extend union DogFish = Dog"""
    val test3Res = UnionTypeExtension(Name("DogFish"), Nil, List(NamedType(Name("Dog"))))

    assert(clue(unionTypeDefinition.parse(test1)) == Right("", test1Res))
    assert(clue(unionTypeDefinition.parse(test2)) == Right("", test2Res))
    assert(clue(unionTypeExtension.parse(test3)) == Right("", test3Res))
  }

  test("enums") {
    val test1 = """enum Direction {
      NORTH
      EAST
      SOUTH
      WEST
    }"""
    val test1Res = EnumTypeDefinition(
      Name("Direction"),
      Nil,
      List(
        EnumValueDefinition(EnumValue(Name("NORTH")), Nil),
        EnumValueDefinition(EnumValue(Name("EAST")), Nil),
        EnumValueDefinition(EnumValue(Name("SOUTH")), Nil),
        EnumValueDefinition(EnumValue(Name("WEST")), Nil)
      )
    )

    val test2 = """extend enum Direction {
      NORTH
      EAST
      SOUTH
      WEST
    }"""
    val test2Res = EnumTypeExtension(
      Name("Direction"),
      Nil,
      List(
        EnumValueDefinition(EnumValue(Name("NORTH")), Nil),
        EnumValueDefinition(EnumValue(Name("EAST")), Nil),
        EnumValueDefinition(EnumValue(Name("SOUTH")), Nil),
        EnumValueDefinition(EnumValue(Name("WEST")), Nil)
      )
    )

    assert(clue(enumTypeDefinition.parse(test1)) == Right("", test1Res))
    assert(clue(enumTypeExtension.parse(test2)) == Right("", test2Res))
  }

  test("input objects") {
    val test1 = """input Point2D {
      x: Float
      y: Float
    }"""
    val test1Res = InputObjectTypeDefinition(
      Name("Point2D"),
      Nil,
      List(
        InputValueDefinition(Name("x"), NamedType(Name("Float")), None, Nil),
        InputValueDefinition(Name("y"), NamedType(Name("Float")), None, Nil)
      )
    )
    val test2 = """input Example {
      self: [Example!]!
      value: String
    }"""
    val test2Res = InputObjectTypeDefinition(
      Name("Example"),
      Nil,
      List(
        InputValueDefinition(
          Name("self"),
          NonNullType(ListType(NonNullType(NamedType(Name("Example"))))),
          None,
          Nil
        ),
        InputValueDefinition(Name("value"), NamedType(Name("String")), None, Nil)
      )
    )
    val test3 = """extend input Point2D {
      x: Float
      y: Float
    }"""
    val test3Res = InputObjectTypeExtension(
      Name("Point2D"),
      Nil,
      List(
        InputValueDefinition(Name("x"), NamedType(Name("Float")), None, Nil),
        InputValueDefinition(Name("y"), NamedType(Name("Float")), None, Nil)
      )
    )

    assert(clue(inputObjectTypeDefinition.parse(test1)) == Right("", test1Res))
    assert(clue(inputObjectTypeDefinition.parse(test2)) == Right("", test2Res))
    assert(clue(inputObjectTypeExtension.parse(test3)) == Right("", test3Res))
  }

  test("directives") {
    val test1 = "directive @example on FIELD"
    val test1Res = DirectiveDefinition(
      Name("example"),
      Nil,
      false,
      NonEmptyList.one(ExecutableDirectiveLocation.FIELD)
    )

    val test2 = "directive @skip(if: Boolean!) on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT"
    val test2Res = DirectiveDefinition(
      Name("skip"),
      List(InputValueDefinition(Name("if"), NonNullType(NamedType(Name("Boolean"))), None, Nil)),
      false,
      NonEmptyList.of(
        ExecutableDirectiveLocation.FIELD,
        ExecutableDirectiveLocation.FRAGMENT_SPREAD,
        ExecutableDirectiveLocation.INLINE_FRAGMENT
      )
    )

    val test3 = """directive @deprecated(
      reason: String = "No longer supported"
    ) on FIELD_DEFINITION | ENUM_VALUE"""
    val test3Res = DirectiveDefinition(
      Name("deprecated"),
      List(
        InputValueDefinition(
          Name("reason"),
          NamedType(Name("String")),
          Some(StringValue("No longer supported")),
          Nil
        )
      ),
      false,
      NonEmptyList.of(
        TypeSystemDirectiveLocation.FIELD_DEFINITION,
        TypeSystemDirectiveLocation.ENUM_VALUE
      )
    )

    assert(clue(directiveDefinition.parse(test1)) == Right("", test1Res))
    assert(clue(directiveDefinition.parse(test2)) == Right("", test2Res))
    assert(clue(directiveDefinition.parse(test3)) == Right("", test3Res))
  }

  test("schema") {
    val test1 = """schema {
      query: MyQueryRootType
      mutation: MyMutationRootType
    }"""
    val test1Res = SchemaDefinition(
      Nil,
      NonEmptyList.of(
        RootOperationTypeDefinition(OperationType.Query, NamedType(Name("MyQueryRootType"))),
        RootOperationTypeDefinition(OperationType.Mutation, NamedType(Name("MyMutationRootType")))
      )
    )

    val test2 = """extend schema {
      query: MyQueryRootType
      mutation: MyMutationRootType
    }"""
    val test2Res = SchemaExtension(
      Nil,
      List(
        RootOperationTypeDefinition(OperationType.Query, NamedType(Name("MyQueryRootType"))),
        RootOperationTypeDefinition(OperationType.Mutation, NamedType(Name("MyMutationRootType")))
      )
    )

    assert(clue(schemaDefinition.parse(test1)) == Right("", test1Res))
    assert(clue(schemaExtension.parse(test2)) == Right("", test2Res))
  }

  test("document") {
    assert(clue(typeSystemDocument.parse(schemaStr)) == Right("", schemaDoc))
  }

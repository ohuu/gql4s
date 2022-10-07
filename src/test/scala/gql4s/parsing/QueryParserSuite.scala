// Copyright (c) 2021 by Oliver Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s

import cats.data.NonEmptyList

import munit.FunSuite
import parsing.*
import parsing.OperationType.*
import parsing.Type.*
import parsing.Value.*

class QueryParserSuite extends FunSuite:
    test("comments") {
        assert(clue(comment.parse("#")).isRight)
        assert(clue(comment.parse("#comment")).isRight)
        assert(clue(comment.parse("# comment 0")).isRight)
        assert(clue(comment.parse("")).isLeft)
    }

    test("names") {
        assert(clue(name.parse("a")) == Right("", Name("a")))
        assert(clue(name.parse("_0myName")) == Right("", Name("_0myName")))
        assert(name.parse("0name").isLeft) // names cannot start with a number
        assert(name.parse("").isLeft)
    }

    test("values") {
        // Int Value
        assert(clue(value.parse("1")) == Right("", IntValue("1")))
        assert(clue(value.parse("-0")) == Right("", IntValue("-0")))
        assert(clue(value.parse("-1338")) == Right("", IntValue("-1338")))
        assert(clue(value.parse("1338")) == Right("", IntValue("1338")))

        // Float Value
        assert(clue(value.parse("0.0")) == Right("", FloatValue("0.0")))
        assert(clue(value.parse("-1.9")) == Right("", FloatValue("-1.9")))
        assert(clue(value.parse("10.55")) == Right("", FloatValue("10.55")))
        assert(clue(value.parse("-42.1338")) == Right("", FloatValue("-42.1338")))
        assert(clue(value.parse("1338e-4")) == Right("", FloatValue("1338e-4")))
        assert(clue(value.parse("42.0E+2")) == Right("", FloatValue("42.0E+2")))
        assert(clue(value.parse(".32")).isLeft)

        // Boolean Value
        assert(clue(value.parse("true")) == Right("", BooleanValue("true")))
        assert(clue(value.parse("false")) == Right("", BooleanValue("false")))

        // String Value
        assert(clue(triQuote.parse("\"\"\"")).isRight)
        assert(clue(blockString.parse("\"\"\"asdf\"\"\"")) == Right("", "asdf"))
        assert(clue(value.parse("\"\"\"abcd\"\"\"")) == Right("", StringValue("abcd")))
        assert(clue(value.parse("\"\"\"\\\"\"\"\"\"\"")) == Right("", StringValue("\\\"\"\"")))
        assert(clue(value.parse("\"\\u01DF\"")) == Right("", StringValue("\\u01DF")))
        assert(clue(value.parse("\"01df\"")) == Right("", StringValue("01df")))
        assert(clue(value.parse("\"\\u01dg\"")).isLeft)
        assert(clue(value.parse("\"\\u01dfa\"")) == Right("", StringValue("\\u01dfa")))
        assert(
            clue(value.parse("\"my name is Oli\"")) == Right("", StringValue("my name is Oli"))
        )
        assert(clue(value.parse("\"\\\"\"")) == Right("", StringValue("\\\"")))
        assert(
            clue(value.parse("\"my name is \\\"Oli\\\"\"")) == Right(
                "",
                StringValue("my name is \\\"Oli\\\"")
            )
        )
        assert(clue(value.parse("\"\\u05AD\"")) == Right("", StringValue("\\u05AD")))

        // Null Value
        assert(clue(value.parse("null")) == Right("", NullValue))

        // Enum Value
        assert(clue(value.parse("Mars")) == Right("", EnumValue(Name("Mars"))))
        assert(clue(enumValue.parse("true")).isLeft)
        assert(clue(enumValue.parse("null")).isLeft)

        // List Value
        val listTest = List(
            "[]"       -> ("", ListValue(Nil)),
            "[42]"     -> ("", ListValue(IntValue("42") :: Nil)),
            "[42, 43]" -> ("", ListValue(IntValue("42") :: IntValue("43") :: Nil)),
            "[[42]]"   -> ("", ListValue(ListValue(IntValue("42") :: Nil) :: Nil))
        )

        assert(clue(value.parse(listTest(0)._1)) == Right(listTest(0)._2))
        assert(clue(value.parse(listTest(1)._1)) == Right(listTest(1)._2))
        assert(clue(value.parse(listTest(2)._1)) == Right(listTest(2)._2))
        assert(clue(value.parse(listTest(3)._1)) == Right(listTest(3)._2))

        // Object Value
        val objTest = List(
            "{}" -> ("", ObjectValue(Nil)),
            """{
          name: "oli"
        }""" -> (
                "",
                ObjectValue(ObjectField(Name("name"), StringValue("oli")) :: Nil)
            )
        )

        assert(clue(value.parse(objTest(0)._1)) == Right(objTest(0)._2))
        assert(clue(value.parse(objTest(1)._1)) == Right(objTest(1)._2))
        assert(clue(value.parse("1338")) == Right("", IntValue("1338")))
        assert(clue(value.parse("-42.0E+2")) == Right("", FloatValue("-42.0E+2")))
        assert(clue(value.parse("true")) == Right("", BooleanValue("true")))
        assert(
            clue(value.parse("\"name \\\"Oli\\\"\"")) == Right("", StringValue("name \\\"Oli\\\""))
        )
        assert(clue(value.parse("null")) == Right("", NullValue))
        assert(clue(value.parse("Mars")) == Right("", EnumValue(Name("Mars"))))
    }

    test("type references") {
        assert(clue(listType.parse("[Int]")) == Right("", ListType(NamedType(Name("Int")))))
        assert(clue(listType.parse("[42]")).isLeft)
        assert(clue(`type`.parse("[Int]")) == Right("", ListType(NamedType(Name("Int")))))
        assert(clue(`type`.parse("Int!")) == Right("", NonNullType(NamedType(Name("Int")))))
        assert(clue(`type`.parse("[Int]!")) == Right("", NonNullType(ListType(NamedType(Name("Int"))))))
    }

    test("variables") {
        assert(clue(variable.parse("$thing")) == Right("", Variable(Name("thing"))))
        // variableDefinition.parse("$thing: Int! = 42 @skip") match
        //   case Right(asdf, _) => println(asdf)
        //   case _              => println("fuck you")

        variableDefinition.parse("$thing: Int! = 42")
        assert(
            clue(variableDefinition.parse("$thing: Int! = 42")) ==
                Right(
                    "",
                    VariableDefinition(
                        Name("thing"),
                        NonNullType(NamedType(Name("Int"))),
                        Some(IntValue("42")),
                        Nil
                    )
                )
        )
    }

    test("arguments") {
        val test = List(
            "thing: 42.5" -> ("", Argument(Name("thing"), FloatValue("42.5"))),
            """( name: "oli", age: 38  )""" -> (
                "",
                NonEmptyList(
                    Argument(Name("name"), StringValue("oli")),
                    Argument(Name("age"), IntValue("38")) :: Nil
                )
            )
        )

        val test2    = "id: 4"
        val test2Res = Argument(Name("id"), IntValue("4"))

        assert(clue(argument.parse(test(0)._1)) == Right(test(0)._2))
        assert(clue(arguments.parse(test(1)._1)) == Right(test(1)._2))
        assert(clue(argument.parse(test2)) == Right("", test2Res))
    }

    test("directives") {
        val test = List(
            """@excludeField(name: "photo")""" -> (
                "",
                Directive(
                    Name("excludeField"),
                    List(
                        Argument(Name("name"), StringValue("photo"))
                    )
                )
            )
        )

        // Directive
        assert(clue(directive.parse(test(0)._1)) == Right(test(0)._2))
    }

    test("fragments") {
        val field0    = "field"
        val field0Res = Field(None, Name("field"), Nil, Nil, Nil)

        val field1 = """field {
        name
      }"""
        val field1Res = Field(
            None,
            Name("field"),
            Nil,
            Nil,
            List(Field(None, Name("name"), Nil, Nil, Nil))
        )

        val field2 = """user(id:4) {
        name
      }"""
        val field2Res = Field(
            None,
            Name("user"),
            List(Argument(Name("id"), IntValue("4"))),
            Nil,
            List(Field(None, Name("name"), Nil, Nil, Nil))
        )

        val field3 = """user(id:4) { # this comment used to cause problems
        name
      }"""
        val field3Res = Field(
            None,
            Name("user"),
            List(Argument(Name("id"), IntValue("4"))),
            Nil,
            List(Field(None, Name("name"), Nil, Nil, Nil))
        )

        // TODO: test field with directives

        val fragmentDefinition0 = """fragment friendFields on User {
        id
        name
        ...standardProfilePic
      }"""
        val fragmentDefinition0Res = FragmentDefinition(
            Name("friendFields"),
            NamedType(Name("User")),
            Nil,
            List(
                Field(None, Name("id"), Nil, Nil, Nil),
                Field(None, Name("name"), Nil, Nil, Nil),
                FragmentSpread(Name("standardProfilePic"), Nil)
            )
        )

        val fragment1Definition = """fragment friendFields on User {
        id
        name
        ...mySpread
        ... on Thing {
          id
        }
      }"""
        val fragment1DefinitionRes = FragmentDefinition(
            Name("friendFields"),
            NamedType(Name("User")),
            Nil,
            List(
                Field(None, Name("id"), Nil, Nil, Nil),
                Field(None, Name("name"), Nil, Nil, Nil),
                FragmentSpread(Name("mySpread"), Nil),
                InlineFragment(
                    Some(NamedType(Name("Thing"))),
                    Nil,
                    List(Field(None, Name("id"), Nil, Nil, Nil))
                )
            )
        )

        assert(clue(fragmentSpread.parse("...thing")) == Right("", FragmentSpread(Name("thing"), Nil)))
        assert(clue(fragmentSpread.parse("...on")).isLeft)
        assert(clue(field.parse(field0)) == Right("", field0Res))
        assert(clue(field.parse(field1)) == Right("", field1Res))
        assert(clue(field.parse(field2)) == Right("", field2Res))
        assert(clue(field.parse(field3)) == Right("", field3Res))
        assert(clue(fragmentDefinition.parse(fragmentDefinition0)) == Right("", fragmentDefinition0Res))
        assert(clue(fragmentDefinition.parse(fragment1Definition)) == Right("", fragment1DefinitionRes))
    }

    test("operations") {
        val mutation = """mutation {
        likeStory(storyID: 12345) {
          story {
            likeCount
          }
        }
      }"""

        val mutationRes = OperationDefinition(
            Name(""),
            Mutation,
            variableDefinitions = Nil,
            directives = Nil,
            selectionSet = List(
                Field(
                    alias = None,
                    name = Name("likeStory"),
                    arguments = List(Argument(Name("storyID"), IntValue("12345"))),
                    directives = Nil,
                    selectionSet = List(
                        Field(
                            alias = None,
                            name = Name("story"),
                            arguments = Nil,
                            directives = Nil,
                            selectionSet = List(
                                Field(
                                    alias = None,
                                    name = Name("likeCount"),
                                    arguments = Nil,
                                    directives = Nil,
                                    selectionSet = Nil
                                )
                            )
                        )
                    )
                )
            )
        )

        val anonQuery = """{
        field
      }"""

        val anonQueryRes = OperationDefinition(
            Name(""),
            Query,
            Nil,
            Nil,
            List(Field(None, Name("field"), Nil, Nil, Nil))
        )

        assert(clue(operationType.parse("mutation ")).isRight)
        assert(clue(operationDefinition.parse(mutation)) == Right("", mutationRes))
        assert(clue(operationDefinition.parse(anonQuery)) == Right("", anonQueryRes))
    }
end QueryParserSuite

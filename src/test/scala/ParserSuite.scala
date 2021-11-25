package parser

import adt.*
import adt.OperationType.*
import adt.Value.*
import adt.Selection.*
import adt.Type.*
import cats.data.NonEmptyList
import munit.FunSuite

class ParserSuite extends FunSuite:
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
    assert(clue(value.parse("1")) == Right("", IntValue(1)))
    assert(clue(value.parse("-0")) == Right("", IntValue(0)))
    assert(clue(value.parse("-1338")) == Right("", IntValue(-1338)))
    assert(clue(value.parse("1338")) == Right("", IntValue(1338)))

    // Float Value
    assert(clue(value.parse("0.0")) == Right("", FloatValue(0)))
    assert(clue(value.parse("-1.9")) == Right("", FloatValue(-1.9)))
    assert(clue(value.parse("10.55")) == Right("", FloatValue(10.55)))
    assert(clue(value.parse("-42.1338")) == Right("", FloatValue(-42.1338)))
    assert(clue(value.parse("1338e-4")) == Right("", FloatValue(1338e-4)))
    assert(clue(value.parse("42.0E+2")) == Right("", FloatValue(42.0e+2)))
    assert(clue(value.parse(".32")).isLeft)

    // Boolean Value
    assert(clue(value.parse("true")) == Right("", BooleanValue(true)))
    assert(clue(value.parse("false")) == Right("", BooleanValue(false)))

    // String Value
    val strTest = List(
      "\"\"\"abcd\"\"\""           -> ("", "abcd"),
      "\"\"\"\\\"\"\"\"\"\""       -> ("", "\\\"\"\""),
      "01DF"                       -> ("", "01DF"),
      "01df"                       -> ("", "01df"),
      "01dfa"                      -> ("a", "01df"),
      "\"my name is Oli\""         -> ("", StringValue("my name is Oli")),
      "\\\""                       -> ("", "\\\""),
      "\"my name is \\\"Oli\\\"\"" -> ("", StringValue("my name is \\\"Oli\\\"")),
      "\"\\u05AD\""                -> ("", StringValue("\\u05AD"))
    )

    assert(clue(blockStringValue.parse(strTest(0)._1)) == Right(strTest(0)._2))
    assert(clue(blockStringValue.parse(strTest(1)._1)) == Right(strTest(1)._2))
    assert(clue(escapedUnicode.parse(strTest(2)._1)) == Right(strTest(2)._2))
    assert(clue(escapedUnicode.parse(strTest(3)._1)) == Right(strTest(3)._2))
    assert(clue(escapedUnicode.parse(strTest(4)._1)) == Right(strTest(4)._2))
    assert(clue(escapedUnicode.parse("01dg")).isLeft)
    assert(clue(stringValue.parse(strTest(5)._1)) == Right(strTest(5)._2))
    assert(clue(stringValue1.parse(strTest(6)._1)) == Right(strTest(6)._2))
    assert(clue(stringValue.parse(strTest(7)._1)) == Right(strTest(7)._2))
    assert(clue(stringValue.parse(strTest(8)._1)) == Right(strTest(8)._2))

    // Null Value
    assert(clue(nullValue.parse("null")) == Right("", NullValue))

    // Enum Value
    assert(clue(enumValue.parse("Mars")) == Right("", EnumValue(Name("Mars"))))
    assert(clue(enumValue.parse("true")).isLeft)
    assert(clue(enumValue.parse("null")).isLeft)

    // List Value
    val listTest = List(
      "[]"       -> ("", ListValue(Nil)),
      "[42]"     -> ("", ListValue(IntValue(42) :: Nil)),
      "[42, 43]" -> ("", ListValue(IntValue(42) :: IntValue(43) :: Nil)),
      "[[42]]"   -> ("", ListValue(ListValue(IntValue(42) :: Nil) :: Nil))
    )

    assert(clue(listValue.parse(listTest(0)._1)) == Right(listTest(0)._2))
    assert(clue(listValue.parse(listTest(1)._1)) == Right(listTest(1)._2))
    assert(clue(listValue.parse(listTest(2)._1)) == Right(listTest(2)._2))
    assert(clue(listValue.parse(listTest(3)._1)) == Right(listTest(3)._2))

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

    assert(clue(objectValue.parse(objTest(0)._1)) == Right(objTest(0)._2))
    assert(clue(objectValue.parse(objTest(1)._1)) == Right(objTest(1)._2))

    // Value
    assert(clue(value.parse("1338")) == Right("", IntValue(1338)))
    assert(clue(value.parse("-42.0E+2")) == Right("", FloatValue(-42.0e+2)))
    assert(clue(value.parse("true")) == Right("", BooleanValue(true)))
    assert(clue(value.parse("\"name \\\"Oli\\\"\"")) == Right("", StringValue("name \\\"Oli\\\"")))
    assert(clue(value.parse("null")) == Right("", NullValue))
    assert(clue(value.parse("Mars")) == Right("", EnumValue(Name("Mars"))))
  }

  test("type references") {
    // Type Reference
    assert(clue(listType.parse("[Int]")) == Right("", ListType(NamedType(Name("Int")))))
    assert(clue(listType.parse("[42]")).isLeft)
    assert(clue(`type`.parse("[Int]")) == Right("", ListType(NamedType(Name("Int")))))
    assert(clue(`type`.parse("Int!")) == Right("", NonNullType(Name("Int"))))
  }

  test("variables") {
    // Variableiable
    assert(clue(variable.parse("$thing")) == Right("", Variable(Name("thing"))))
    assert(
      clue(variableDefinition.parse("$thing: Int! = 42")) ==
        Right("", VariableDefinition(Name("thing"), NonNullType(Name("Int")), Some(IntValue(42))))
    )
  }

  test("arguments") {
    val test = List(
      "thing: 42.5" -> ("", Argument(Name("thing"), FloatValue(42.5))),
      """( name: "oli", age: 38  )""" -> (
        "",
        NonEmptyList(
          Argument(Name("name"), StringValue("oli")),
          Argument(Name("age"), IntValue(38)) :: Nil
        )
      )
    )

    val test2    = "id: 4"
    val test2Res = Argument(Name("id"), IntValue(4))

    // Argument
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
      List(Argument(Name("id"), IntValue(4))),
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
      NonEmptyList(
        Field(None, Name("id"), Nil, Nil, Nil),
        List(
          Field(None, Name("name"), Nil, Nil, Nil),
          FragmentSpread(Name("standardProfilePic"), Nil)
        )
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
      NonEmptyList(
        Field(None, Name("id"), Nil, Nil, Nil),
        List(
          Field(None, Name("name"), Nil, Nil, Nil),
          FragmentSpread(Name("mySpread"), Nil),
          InlineFragment(
            Some(NamedType(Name("Thing"))),
            Nil,
            NonEmptyList.one(Field(None, Name("id"), Nil, Nil, Nil))
          )
        )
      )
    )

    assert(clue(fragmentSpread.parse("...thing")) == Right("", FragmentSpread(Name("thing"), Nil)))
    assert(clue(fragmentSpread.parse("...on")).isLeft)
    assert(clue(field.parse(field0)) == Right("", field0Res))
    assert(clue(field.parse(field1)) == Right("", field1Res))
    assert(clue(field.parse(field2)) == Right("", field2Res))
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
      Mutation,
      name = None,
      variableDefinitions = Nil,
      directives = Nil,
      selectionSet = NonEmptyList.one(
        Field(
          alias = None,
          name = Name("likeStory"),
          arguments = List(Argument(Name("storyID"), IntValue(12345))),
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
    val anonQueryRes =
      OperationDefinition(
        Query,
        None,
        Nil,
        Nil,
        NonEmptyList.one(Field(None, Name("field"), Nil, Nil, Nil))
      )

    assert(clue(operationType.parse("mutation ")).isRight)
    assert(clue(operationDefinition.parse(mutation)) == Right("", mutationRes))
    assert(clue(operationDefinition.parse(anonQuery)) == Right("", anonQueryRes))
  }

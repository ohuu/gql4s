package parser

import adt.*
import adt.Value.*
import munit.FunSuite

class ParserSuite extends FunSuite:
  test("comments") {
    assert(clue(comment.parse("# comment 0")).isRight)
  }

  test("names") {
    assert(clue(name.parse("_0myName")) == Right("", Name("_0myName")))
    assert(name.parse("0name").isLeft) // names cannot start with a number
  }

  test("values") {
    // Int Value
    assert(clue(integerPart.parse("-0")) == Right("", "-0"))
    assert(clue(integerPart.parse("-1338")) == Right("", "-1338"))
    assert(clue(intValue.parse("1338")) == Right("", IntValue(1338)))

    // Float Value
    assert(clue(exponentPart.parse("e-42")) == Right("", "e-42"))
    assert(clue(fractionalPart.parse(".1337")) == Right("", ".1337"))
    assert(clue(floatValue.parse("-42.1338")) == Right("", FloatValue(-42.1338)))
    assert(clue(floatValue.parse("1338e-4")) == Right("", FloatValue(1338e-4)))
    assert(clue(floatValue.parse("-42.0E+2")) == Right("", FloatValue(-42.0e+2)))

    // Boolean Value
    assert(clue(booleanValue.parse("true")) == Right("", BooleanValue(true)))
    assert(clue(booleanValue.parse("false")) == Right("", BooleanValue(false)))
    assert(clue(booleanValue.parse("tru")).isLeft)

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
    assert(clue(listType.parse("[Int]")) == Right("", "[Int]"))
    assert(clue(listType.parse("[42]")).isLeft)
    assert(clue(nonNullType.parse("Int!")) == Right("", "Int!"))
    assert(clue(tpe.parse("[Int]")) == Right("", "[Int]"))
    assert(clue(tpe.parse("Int!")) == Right("", "Int!"))
  }

  test("variables") {
    // Variable
    assert(clue(variableDefinition.parse("$thing:Int!=42")).isRight)
  }

  test("arguments") {
    val test = List(
      "thing: 42.5" -> ("", Argument(Name("thing"), FloatValue(42.5))),
      """( name: "oli", age: 38  )""" -> (
        "",
        List(
          Argument(Name("name"), StringValue("oli")),
          Argument(Name("age"), IntValue(38))
        )
      )
    )

    // Argument
    assert(clue(argument.parse(test(0)._1)) == Right(test(0)._2))
    assert(clue(arguments.parse(test(1)._1)) == Right(test(1)._2))
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

package parser

import adt.*
import adt.Value.*
import munit.FunSuite

class ParserSuite extends FunSuite:
  test("test comments") {
    // Comments
    assert(clue(comment.parse("# comment 0")).isRight)

    // Names
    assert(clue(name.parse("_0myName")) == Right("", Name("_0myName")))
    assert(name.parse("0name").isLeft) // names cannot start with a number

    // Int Value
    // assert(clue(integerPart0.parse("-0")) == Right("", "-0"))
    // assert(clue(integerPart1.parse("-1337")) == Right("", "-1337"))
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
    assert(clue(blockStringValue.parse("\"\"\"abcd\"\"\"")) == Right("", "abcd"))
    assert(clue(blockStringValue.parse("\"\"\"\\\"\"\"\"\"\"")) == Right("", "\\\"\"\""))
    assert(clue(escapedUnicode.parse("01DF")) == Right("", "01DF"))
    assert(clue(escapedUnicode.parse("01df")) == Right("", "01df"))
    assert(clue(escapedUnicode.parse("01dfa")) == Right("a", "01df"))
    assert(clue(escapedUnicode.parse("01dg")).isLeft)
    assert(
      clue(stringValue.parse("\"my name is Oli\"")) == Right("", StringValue("my name is Oli"))
    )
    assert(clue(stringValue1.parse("\\\"")) == Right("", "\\\""))
    assert(
      clue(stringValue.parse("\"my name is \\\"Oli\\\"\"")) ==
        Right("", StringValue("my name is \\\"Oli\\\""))
    )
    assert(clue(stringValue.parse("\"\\u05AD\"")) == Right("", StringValue("\\u05AD")))

    // Null Value
    assert(clue(nullValue.parse("null")) == Right("", NullValue))

    // Enum Value
    assert(clue(enumValue.parse("Mars")) == Right("", EnumValue(Name("Mars"))))
    assert(clue(enumValue.parse("true")).isLeft)
    assert(clue(enumValue.parse("null")).isLeft)

    // List Value
    assert(clue(listValue.parse("[]")) == Right("", ListValue(Nil)))
    assert(clue(listValue.parse("[42]")) == Right("", ListValue(IntValue(42) :: Nil)))
    assert(
      clue(listValue.parse("[42, 43]")) == Right("", ListValue(IntValue(42) :: IntValue(43) :: Nil))
    )
    assert(
      clue(listValue.parse("[[42]]")) == Right("", ListValue(ListValue(IntValue(42) :: Nil) :: Nil))
    )

    // Object Value
    assert(clue(objectValue.parse("{}")) == Right("", ObjectValue(Nil)))
    assert(
      clue(objectValue.parse("""{ 
        name: "oli" 
      }""")) == // TODO: Should be able to insert new lines
        Right("", ObjectValue(ObjectField(Name("name"), StringValue("oli")) :: Nil))
    )

    // Value
    assert(clue(value.parse("1338")) == Right("", IntValue(1338)))
    assert(clue(value.parse("-42.0E+2")) == Right("", FloatValue(-42.0e+2)))
    assert(clue(value.parse("true")) == Right("", BooleanValue(true)))
    assert(clue(value.parse("\"name \\\"Oli\\\"\"")) == Right("", StringValue("name \\\"Oli\\\"")))
    assert(clue(value.parse("null")) == Right("", NullValue))
    assert(clue(value.parse("Mars")) == Right("", EnumValue(Name("Mars"))))

    // Type Reference
    assert(clue(listType.parse("[Int]")) == Right("", "[Int]"))
    assert(clue(listType.parse("[42]")).isLeft)
    assert(clue(nonNullType.parse("Int!")) == Right("", "Int!"))
    assert(clue(tpe.parse("[Int]")) == Right("", "[Int]"))
    assert(clue(tpe.parse("Int!")) == Right("", "Int!"))

    // Variable
    assert(clue(variableDefinition.parse("$thing:Int!=42")).isRight)
  }

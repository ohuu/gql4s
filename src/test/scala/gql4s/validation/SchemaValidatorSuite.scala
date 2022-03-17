// Copyright (c) 2021 by Oliver Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s
package validation

import munit.FunSuite
import GqlError.*
import SchemaValidator.*
import Type.*

class SchemaValidatorSuite extends FunSuite:
  test("Any type is a subtype of itself") {
    val schemaStr = """
    interface A {
      a: Int
    }
    """

    typeSystemDocument.parse(schemaStr) match
      case Right(_ -> schema) =>
        val A    = schema.findInterfaceTypeDef(NamedType(Name("A"))).get
        val errs = isValidImplementation(A, A, schema)
        assertEquals(clue(errs), Nil)
      case _ => fail("failed to parse schemaStr")
  }

  test("An implementing type must implement all the types that the implemted type implements") {
    val schemaStr = """
    interface A {
      a: Int
    }

    interface B implements A {
      a: Int
      b: Int
    }

    interface C implements B {
      a: Int
      b: Int
      c: Int
    }
    """

    typeSystemDocument.parse(schemaStr) match
      case Right(_ -> schema) =>
        val A = schema.findInterfaceTypeDef(NamedType(Name("A"))).get
        val B = schema.findInterfaceTypeDef(NamedType(Name("B"))).get
        val C = schema.findInterfaceTypeDef(NamedType(Name("C"))).get

        val noErrs = isValidImplementation(B, A, schema)
        assertEquals(clue(noErrs), Nil)

        val errs        = isValidImplementation(C, B, schema)
        val actualErr   = errs.find(_.isInstanceOf[NonImplementedInterface])
        val expectedErr = NonImplementedInterface(NamedType(Name("A")))
        assertEquals(clue(actualErr), clue(Some(expectedErr)))
      case _ => fail("failed to parse schemaStr")
  }

  test("An implementing type must contain all fields in implemented type") {
    val schemaStr = """
    interface A {
      a: Int
      b: String
    }

    interface B implements A {
      a: Int
      b: String
    }

    interface C implements A {
      a: Int
    }
    """

    typeSystemDocument.parse(schemaStr) match
      case Right(_ -> schema) =>
        val A = schema.findInterfaceTypeDef(NamedType(Name("A"))).get
        val B = schema.findInterfaceTypeDef(NamedType(Name("B"))).get
        val C = schema.findInterfaceTypeDef(NamedType(Name("C"))).get

        val noErrs = isValidImplementation(B, A, schema)
        assertEquals(clue(noErrs), Nil)

        val errs        = isValidImplementation(C, A, schema)
        val actualErr   = errs.find(_.isInstanceOf[MissingField])
        val expectedErr = MissingField(Name("b"), NamedType(Name("C")))
        assertEquals(clue(actualErr), clue(Some(expectedErr)))
      case _ => fail("failed to parse schemaStr")
  }

  test("Each implemented field in the implementing type must return the a valid type") {
    val schemaStr = """
    interface A {
      a: Int
    }

    interface B implements A {
      a: Int
      b: Int
    }

    interface C implements A {
      a: String
    }

    interface X {
      a: A
      u: U
    }

    type Y implements X {
      a: B
      u: W
    }

    type V {
      v: Int
    }
    type W {
      w: String
    }
    union U = V | W
    """

    typeSystemDocument.parse(schemaStr) match
      case Right(_ -> schema) =>
        val A = schema.findInterfaceTypeDef(NamedType(Name("A"))).get
        val C = schema.findInterfaceTypeDef(NamedType(Name("C"))).get
        val X = schema.findInterfaceTypeDef(NamedType(Name("X"))).get
        val Y = schema.findObjLikeTypeDef(NamedType(Name("Y"))).get

        val noErrs = isValidImplementation(Y, X, schema)
        assertEquals(clue(noErrs), Nil)

        val errs        = isValidImplementation(C, A, schema)
        val actualErr   = errs.find(_.isInstanceOf[InvalidImplementation])
        val expectedErr = InvalidImplementation(NamedType(Name("String")))
        assertEquals(clue(actualErr), clue(Some(expectedErr)))

      case _ => fail("failed to parse schemaStr")
  }

  test("All implemented fields with args must have at least the args of the implemented fields") {
    val schemaStr = """
    interface A {
      a(x: Int): String
    }

    interface B implements A {
      a(x: Int): String
    }

    interface C implements A {
      a(x: Int, y: Boolean): String
    }

    interface D implements A {
      a(y: Boolean): String
    }

    interface E implements A {
      a(x: String): String
    }
    """

    typeSystemDocument.parse(schemaStr) match
      case Right(_ -> schema) =>
        val A = schema.findInterfaceTypeDef(NamedType(Name("A"))).get
        val B = schema.findInterfaceTypeDef(NamedType(Name("B"))).get
        val C = schema.findInterfaceTypeDef(NamedType(Name("C"))).get
        val D = schema.findInterfaceTypeDef(NamedType(Name("D"))).get
        val E = schema.findInterfaceTypeDef(NamedType(Name("E"))).get

        val noErrs1 = isValidImplementation(B, A, schema)
        assertEquals(clue(noErrs1), Nil)

        val noErrs2 = isValidImplementation(C, A, schema)
        assertEquals(clue(noErrs2), Nil)

        val errs1        = isValidImplementation(D, A, schema)
        val actualErr1   = errs1.find(_.isInstanceOf[MissingArgument])
        val expectedErr1 = MissingArgument(Name("x"), Name("a"), NamedType(Name("D")))
        assertEquals(clue(actualErr1), clue(Some(expectedErr1)))

        val errs2      = isValidImplementation(E, A, schema)
        val actualErr2 = errs2.find(_.isInstanceOf[IllegalType])
        val expectedErr2 =
          IllegalType(NamedType(Name("String")), Some("expected NamedType(Name(Int))"))
        assertEquals(clue(actualErr2), clue(Some(expectedErr2)))

      case _ => fail("failed to parse schemaStr")
  }

  test(
    "The implementing type may define more arguments on any field as long as those arguments are not required"
  ) {
    val schemaStr = """
    interface A {
      a: String
      b(x: Int): Int
    }

    interface B implements A {
      a(x: String): String
      b(x: Int, y: Boolean): Int
    }

    interface C implements A {
      a(x: String!): String
      b(x: Int, y: Boolean!): Int
    }
    """

    typeSystemDocument.parse(schemaStr) match
      case Right(_ -> schema) =>
        val A = schema.findInterfaceTypeDef(NamedType(Name("A"))).get
        val B = schema.findInterfaceTypeDef(NamedType(Name("B"))).get
        val C = schema.findInterfaceTypeDef(NamedType(Name("C"))).get

        val noErrs = isValidImplementation(B, A, schema)
        assertEquals(clue(noErrs), Nil)

        val errs      = isValidImplementation(C, A, schema)
        val actualErr = errs.filter(_.isInstanceOf[IllegalType])
        val expectedErr = List(
          IllegalType(NonNullType(NamedType(Name("String"))), Some("Argument cannot be required")),
          IllegalType(NonNullType(NamedType(Name("Boolean"))), Some("Argument cannot be required"))
        )
        assertEquals(clue(actualErr), clue(expectedErr))

      case _ => fail("failed to parse schemaStr")
  }

  test(
    "Fields that return nullable types may be implemented by fields that return non-nullable types"
  ) {
    val schemaStr = """
    interface A {
      a: String
    }

    interface B {
      a: String!
    }
    """
    typeSystemDocument.parse(schemaStr) match
      case Right(_ -> schema) =>
        val A = schema.findInterfaceTypeDef(NamedType(Name("A"))).get
        val B = schema.findInterfaceTypeDef(NamedType(Name("B"))).get

        val noErrs = isValidImplementation(B, A, schema)
        assertEquals(clue(noErrs), Nil)

      case _ => fail("failed to parse schemaStr")
  }

  test("Object types must define one or more fields") {
    val schemaStr = """
    type A {}
    """
    typeSystemDocument.parse(schemaStr) match
      case Right(_ -> schema) =>
        val A           = schema.findObjTypeDef(NamedType(Name("A"))).get
        val errs        = validateObjLike(A, schema)
        val actualErr   = errs.find(_.isInstanceOf[MissingFields])
        val expectedErr = MissingFields(NamedType(Name("A")))
        assertEquals(clue(actualErr), clue(Some(expectedErr)))

      case _ => fail("failed to parse schemaStr")
  }

  test("Object fields must have unique names within the object") {
    val schemaStr = """
    type A {
      a: Int
      b: String
    }

    type B {
      a: Int
      a: Int
    }
    """
    typeSystemDocument.parse(schemaStr) match
      case Right(_ -> schema) =>
        val A = schema.findObjTypeDef(NamedType(Name("A"))).get
        val B = schema.findObjTypeDef(NamedType(Name("B"))).get

        val noErrs = validateObjLike(A, schema)
        assertEquals(clue(noErrs), Nil)

        val errs        = validateObjLike(B, schema)
        val actualErr   = errs.find(_.isInstanceOf[DuplicateField])
        val expectedErr = DuplicateField(Name("a"))
        assertEquals(clue(actualErr), clue(Some(expectedErr)))

      case _ => fail("failed to parse schemaStr")
  }

  test("Field names must not start with `__`") {
    val schemaStr = """
    type A {
      __a: Int
    }
    """
    typeSystemDocument.parse(schemaStr) match
      case Right(_ -> schema) =>
        val A           = schema.findObjTypeDef(NamedType(Name("A"))).get
        val errs        = validateObjLike(A, schema)
        val actualErr   = errs.find(_.isInstanceOf[IllegalName])
        val expectedErr = IllegalName(Name("__a"))
        assertEquals(clue(actualErr), clue(Some(expectedErr)))

      case _ => fail("failed to parse schemaStr")
  }

  test("Object fields must be output types") {
    val schemaStr = """
      input InputObj {
        in: String
      }
      type A {
        a: InputObj
      }
    """
    typeSystemDocument.parse(schemaStr) match
      case Right(_ -> schema) =>
        val A           = schema.findObjTypeDef(NamedType(Name("A"))).get
        val errs        = validateObjLike(A, schema)
        val actualErr   = errs.find(_.isInstanceOf[InvalidType])
        val expectedErr = InvalidType(NamedType(Name("InputObj")))
        assertEquals(clue(actualErr), clue(Some(expectedErr)))
      case _ => fail("failed to parse schemaStr")
  }

  test("Objects may implement one or more unique interfaces") {
    val schemaStr = """
      interface A {
        a: Int
      }
      type B implements A & A {
        a: Int
      }
    """
    typeSystemDocument.parse(schemaStr) match
      case Right(_ -> schema) =>
        val B           = schema.findObjTypeDef(NamedType(Name("B"))).get
        val errs        = validateObjLike(B, schema)
        val actualErr   = errs.find(_.isInstanceOf[DuplicateInterface])
        val expectedErr = DuplicateInterface(Name("A"))
        assertEquals(clue(actualErr), clue(Some(expectedErr)))
      case _ => fail("failed to parse schemaStr")
  }

  test("Object field arguments must not start with `__`") {
    val schemaStr = """
      type A {
        a(__x: Int): Int
      }
    """
    typeSystemDocument.parse(schemaStr) match
      case Right(_ -> schema) =>
        val A = schema.findObjTypeDef(NamedType(Name("A"))).get

        val errs        = validateObjLike(A, schema)
        val actualErr   = errs.find(_.isInstanceOf[IllegalName])
        val expectedErr = IllegalName(Name("__x"))
        assertEquals(clue(actualErr), clue(Some(expectedErr)))
      case _ => fail("failed to parse schemaStr")
  }

  test("Object field arguments must be an input type") {
    val schemaStr = """
      type Obj {
        o: Int
      }
      type A {
        a(x: Obj): Int
      }
    """
    typeSystemDocument.parse(schemaStr) match
      case Right(_ -> schema) =>
        val A = schema.findObjTypeDef(NamedType(Name("A"))).get

        val errs        = validateObjLike(A, schema)
        val actualErr   = errs.find(_.isInstanceOf[InvalidType])
        val expectedErr = InvalidType(NamedType(Name("Obj")))
        assertEquals(clue(actualErr), clue(Some(expectedErr)))
      case _ => fail("failed to parse schemaStr")
  }
end SchemaValidatorSuite

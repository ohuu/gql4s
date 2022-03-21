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
        val A    = schema.findTypeDef[InterfaceTypeDefinition](Name("A")).get
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
        val A = schema.findTypeDef[InterfaceTypeDefinition](Name("A")).get
        val B = schema.findTypeDef[InterfaceTypeDefinition](Name("B")).get
        val C = schema.findTypeDef[InterfaceTypeDefinition](Name("C")).get

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
        val A = schema.findTypeDef[InterfaceTypeDefinition](Name("A")).get
        val B = schema.findTypeDef[InterfaceTypeDefinition](Name("B")).get
        val C = schema.findTypeDef[InterfaceTypeDefinition](Name("C")).get

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
        val A = schema.findTypeDef[InterfaceTypeDefinition](Name("A")).get
        val C = schema.findTypeDef[InterfaceTypeDefinition](Name("C")).get
        val X = schema.findTypeDef[InterfaceTypeDefinition](Name("X")).get
        val Y = schema.findTypeDef[ObjectLikeTypeDefinition](Name("Y")).get

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
        val A = schema.findTypeDef[InterfaceTypeDefinition](Name("A")).get
        val B = schema.findTypeDef[InterfaceTypeDefinition](Name("B")).get
        val C = schema.findTypeDef[InterfaceTypeDefinition](Name("C")).get
        val D = schema.findTypeDef[InterfaceTypeDefinition](Name("D")).get
        val E = schema.findTypeDef[InterfaceTypeDefinition](Name("E")).get

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
        val A = schema.findTypeDef[InterfaceTypeDefinition](Name("A")).get
        val B = schema.findTypeDef[InterfaceTypeDefinition](Name("B")).get
        val C = schema.findTypeDef[InterfaceTypeDefinition](Name("C")).get

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
        val A = schema.findTypeDef[InterfaceTypeDefinition](Name("A")).get
        val B = schema.findTypeDef[InterfaceTypeDefinition](Name("B")).get

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
        val A           = schema.findTypeDef[ObjectTypeDefinition](Name("A")).get
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
        val A = schema.findTypeDef[ObjectTypeDefinition](Name("A")).get
        val B = schema.findTypeDef[ObjectTypeDefinition](Name("B")).get

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
        val A           = schema.findTypeDef[ObjectTypeDefinition](Name("A")).get
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
        val A           = schema.findTypeDef[ObjectTypeDefinition](Name("A")).get
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
        val B           = schema.findTypeDef[ObjectTypeDefinition](Name("B")).get
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
        val A = schema.findTypeDef[ObjectTypeDefinition](Name("A")).get

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
        val A = schema.findTypeDef[ObjectTypeDefinition](Name("A")).get

        val errs        = validateObjLike(A, schema)
        val actualErr   = errs.find(_.isInstanceOf[InvalidType])
        val expectedErr = InvalidType(NamedType(Name("Obj")))
        assertEquals(clue(actualErr), clue(Some(expectedErr)))
      case _ => fail("failed to parse schemaStr")
  }

  test("An interface must not implement itself") {
    val schemaStr = """
    interface A implements A {
      a: Int!
    }
    """
    typeSystemDocument.parse(schemaStr) match
      case Right(_ -> schema) =>
        val A = schema.findTypeDef[InterfaceTypeDefinition](Name("A")).get

        val errs        = validateObjLike(A, schema)
        val actualErr   = errs.find(_.isInstanceOf[SelfImplementation])
        val expectedErr = SelfImplementation(NamedType(Name("A")))
        assertEquals(clue(actualErr), clue(Some(expectedErr)))

      case _ => fail("failed to parse schemaStr")
  }

  test("A union must include one or more unique member types") {
    val schemaStr = """
    type A {
      a: Int!
    }
    type B {
      b: String
    }
    union C = A | B
    union D = A | A
    """
    typeSystemDocument.parse(schemaStr) match
      case Right(_ -> schema) =>
        val C = schema.findTypeDef[UnionTypeDefinition](Name("C")).get
        val D = schema.findTypeDef[UnionTypeDefinition](Name("D")).get

        val noErrs = validateUnion(C, schema)
        assertEquals(clue(noErrs), Nil)

        val errs        = validateUnion(D, schema)
        val actualErr   = errs.find(_.isInstanceOf[DuplicateInterface])
        val expectedErr = DuplicateInterface(Name("A"))
        assertEquals(clue(actualErr), clue(Some(expectedErr)))

      case _ => fail("failed to parse schemaStr")
  }

  test("Enums must include one or more unique values") {
    val schemaStr = """
    enum Planets {
      MARS
      MARS
    }
    """
    typeSystemDocument.parse(schemaStr) match
      case Right(_ -> schema) =>
        val planets     = schema.findTypeDef[EnumTypeDefinition](Name("Planets")).get
        val errs        = validateEnum(planets, schema)
        val actualErr   = errs.find(_.isInstanceOf[DuplicateValue])
        val expectedErr = DuplicateValue(Name("MARS"))
        assertEquals(clue(actualErr), clue(Some(expectedErr)))

      case _ => fail("failed to parse schemaStr")
  }

  test("Input objects must contain one or more fields") {
    val schemaStr = """
    input A {}
    """
    typeSystemDocument.parse(schemaStr) match
      case Right(_ -> schema) => fail("Should not have parsed.")
      case Left(_)            => assert(1 == 1)
  }

  test("Input object fields must have valid name") {
    val schemaStr = """
    input A {
      __a: Int
    }
    """
    typeSystemDocument.parse(schemaStr) match
      case Right(_ -> schema) =>
        val A           = schema.findTypeDef[InputObjectTypeDefinition](Name("A")).get
        val errs        = validateInputObj(A, schema)
        val actualErr   = errs.find(_.isInstanceOf[IllegalName])
        val expectedErr = IllegalName(Name("__a"))
        assertEquals(clue(actualErr), clue(Some(expectedErr)))
      case _ => fail("failed to parse schemaStr")
  }

  test("Input object fields must be a valid input field type") {
    val schemaStr = """
    type Obj {
      a: String
    }

    input InObj {
      a: Int
    }

    input A {
      a: InObj
    }

    input B {
      b: Obj
    }
    """
    typeSystemDocument.parse(schemaStr) match
      case Right(_ -> schema) =>
        val A = schema.findTypeDef[InputObjectTypeDefinition](Name("A")).get
        val B = schema.findTypeDef[InputObjectTypeDefinition](Name("B")).get

        val noErrs = validateInputObj(A, schema)
        assertEquals(clue(noErrs), Nil)

        val errs        = validateInputObj(B, schema)
        val actualErr   = errs.find(_.isInstanceOf[InvalidType])
        val expectedErr = InvalidType(NamedType(Name("Obj")))
        assertEquals(clue(actualErr), clue(Some(expectedErr)))
      case _ => fail("failed to parse schemaStr")
  }

  test("Input objects must not contain themselves either directly or indirectly") {
    val schemaStr = """
    input A {
      e: E!
    }

    input B {
      i: Int
    }

    input C {
      b: B
    }
    
    input D {
      d: D!
    }

    input E {
      a: A!
    }
    """
    typeSystemDocument.parse(schemaStr) match
      case Right(_ -> schema) =>
        val errs = validate(schema)
        assertEquals(
          clue(errs),
          Left(
            List(ContainsCycles(Name("A")), ContainsCycles(Name("D")), ContainsCycles(Name("E")))
          )
        )
      case _ => fail("failed to parse schemaStr")
  }
end SchemaValidatorSuite

// Copyright (c) 2021 by Oliver Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s
package validation

import cats.implicits.*
import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import munit.FunSuite
import parsing.*
import errors.*

import SchemaValidator.*
import Type.*
import GqlError.*

class SchemaValidatorSuite extends FunSuite:
  test("Any type is a subtype of itself") {
    val schemaStr = """
    interface A {
      a: Int
    }
    """
    typeSystemDocument.parse(schemaStr) match
      case Right((_, schema @ given TypeSystemDocument)) =>
        val A                = schema.findTypeDef[InterfaceTypeDefinition](Name("A")).get
        val validationResult = validateImplementation(A, A)
        assert(validationResult.isValid)
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
      case Right((_, schema @ given TypeSystemDocument)) =>
        val A = schema.findTypeDef[InterfaceTypeDefinition](Name("A")).get
        val B = schema.findTypeDef[InterfaceTypeDefinition](Name("B")).get
        val C = schema.findTypeDef[InterfaceTypeDefinition](Name("C")).get

        val res1 = validateImplementation(B, A)
        assert(res1.isValid)

        validateImplementation(C, B) match
          case Invalid(errs) =>
            val actualErr   = errs.head
            val expectedErr = MissingImplementations(Name("C"), List(Name("A")))
            assertEquals(clue(actualErr), expectedErr)
          case Valid(_) => fail("Expected to fail")
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
      case Right((_, schema @ given TypeSystemDocument)) =>
        val A = schema.findTypeDef[InterfaceTypeDefinition](Name("A")).get
        val B = schema.findTypeDef[InterfaceTypeDefinition](Name("B")).get
        val C = schema.findTypeDef[InterfaceTypeDefinition](Name("C")).get

        val res1 = validateImplementation(B, A)
        assert(res1.isValid)

        validateImplementation(C, A) match
          case Invalid(errs) =>
            val actualErr   = errs.head
            val expectedErr = MissingName(Name("b"))
            assertEquals(clue(actualErr), expectedErr)
          case Valid(_) => fail("Expected to fail")
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
      case Right((_, schema @ given TypeSystemDocument)) =>
        val A = schema.findTypeDef[InterfaceTypeDefinition](Name("A")).get
        val C = schema.findTypeDef[InterfaceTypeDefinition](Name("C")).get
        val X = schema.findTypeDef[InterfaceTypeDefinition](Name("X")).get
        val Y = schema.findTypeDef[ObjectLikeTypeDefinition](Name("Y")).get

        val noErrs = validateImplementation(Y, X)
        assert(noErrs.isValid)

        validateImplementation(C, A) match
          case Invalid(errs) =>
            val actualErr   = errs.head.asInstanceOf[InvalidType]
            val expectedErr = InvalidType(NamedType(Name("String"))).asInstanceOf[InvalidType]
            assertEquals(clue(actualErr.`type`), expectedErr.`type`)
          case Valid(_) => fail("Expected to fail")
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
      case Right((_, schema @ given TypeSystemDocument)) =>
        val A = schema.findTypeDef[InterfaceTypeDefinition](Name("A")).get
        val B = schema.findTypeDef[InterfaceTypeDefinition](Name("B")).get
        val C = schema.findTypeDef[InterfaceTypeDefinition](Name("C")).get
        val D = schema.findTypeDef[InterfaceTypeDefinition](Name("D")).get
        val E = schema.findTypeDef[InterfaceTypeDefinition](Name("E")).get

        val noErrs1 = validateImplementation(B, A)
        assert(clue(noErrs1.isValid))

        val noErrs2 = validateImplementation(C, A)
        assert(clue(noErrs2.isValid))

        validateImplementation(D, A) match
          case Invalid(errs) =>
            val actualErr   = errs.head
            val expectedErr = MissingArgument2(Name("x"))
            assertEquals(clue(actualErr), expectedErr)
          case Valid(_) => fail("Expected to fail")

        validateImplementation(E, A) match
          case Invalid(errs) =>
            val actualErr   = errs.head
            val expectedErr = InvalidType(NamedType(Name("String")))
            assertEquals(clue(actualErr), expectedErr)
          case Valid(_) => fail("Expected to fail")
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
      case Right((_, schema @ given TypeSystemDocument)) =>
        val A = schema.findTypeDef[InterfaceTypeDefinition](Name("A")).get
        val B = schema.findTypeDef[InterfaceTypeDefinition](Name("B")).get
        val C = schema.findTypeDef[InterfaceTypeDefinition](Name("C")).get

        val noErrs = validateImplementation(B, A)
        assert(noErrs.isValid)

        validateImplementation(C, A) match
          case Invalid(errs) =>
            val actualErrs = errs.toNonEmptyList
            val expectedErrs = NonEmptyList.of(
              InvalidType(NonNullType(NamedType(Name("String")))),
              InvalidType(NonNullType(NamedType(Name("Boolean"))))
            )
            assertEquals(clue(actualErrs), expectedErrs)
          case Valid(_) => fail("Expected to fail")
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
      case Right((_, schema @ given TypeSystemDocument)) =>
        val A = schema.findTypeDef[InterfaceTypeDefinition](Name("A")).get
        val B = schema.findTypeDef[InterfaceTypeDefinition](Name("B")).get

        val noErrs = validateImplementation(B, A)
        assert(noErrs.isValid)

      case _ => fail("failed to parse schemaStr")
  }

  test("Object types must define one or more fields") {
    val schemaStr = """
    type A {}
    """
    typeSystemDocument.parse(schemaStr) match
      case Right((_, schema @ given TypeSystemDocument)) =>
        val A = schema.findTypeDef[ObjectTypeDefinition](Name("A")).get

        validateObjLike(A) match
          case Invalid(errs) =>
            val actualErr   = errs.head
            val expectedErr = StructureEmpty()
            assertEquals(clue(actualErr), expectedErr)
          case Valid(_) => fail("Expected to fail")
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
      case Right((_, schema @ given TypeSystemDocument)) =>
        val A = schema.findTypeDef[ObjectTypeDefinition](Name("A")).get
        val B = schema.findTypeDef[ObjectTypeDefinition](Name("B")).get

        val noErrs = validateObjLike(A)
        assert(noErrs.isValid)

        validateObjLike(B) match
          case Invalid(errs) =>
            val actualErr   = errs.head
            val expectedErr = DuplicateName(Name("a"))
            assertEquals(clue(actualErr), expectedErr)
          case Valid(_) => fail("Expected to fail")
      case _ => fail("failed to parse schemaStr")
  }

  test("Field names must not start with `__`") {
    val schemaStr = """
    type A {
      __a: Int
    }
    """
    typeSystemDocument.parse(schemaStr) match
      case Right((_, schema @ given TypeSystemDocument)) =>
        val A = schema.findTypeDef[ObjectTypeDefinition](Name("A")).get

        validateObjLike(A) match
          case Invalid(errs) =>
            val actualErr   = errs.head
            val expectedErr = InvalidName(Name("__a"))
            assertEquals(clue(actualErr), expectedErr)
          case Valid(_) => fail("Expected to fail")
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
      case Right((_, schema @ given TypeSystemDocument)) =>
        val A = schema.findTypeDef[ObjectTypeDefinition](Name("A")).get
        validateObjLike(A) match
          case Invalid(errs) =>
            val actualErr   = errs.head
            val expectedErr = InvalidType(NamedType(Name("InputObj")))
            assertEquals(clue(actualErr), expectedErr)
          case Valid(_) => fail("Expected to fail")
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
      case Right((_, schema @ given TypeSystemDocument)) =>
        val B = schema.findTypeDef[ObjectTypeDefinition](Name("B")).get
        validateObjLike(B) match
          case Invalid(errs) =>
            val actualErr   = errs.head
            val expectedErr = DuplicateName(Name("A"))
            assertEquals(clue(actualErr), expectedErr)
          case Valid(_) => fail("Expected to fail")
      case _ => fail("failed to parse schemaStr")
  }

  test("Object field arguments must not start with `__`") {
    val schemaStr = """
      type A {
        a(__x: Int): Int
      }
    """
    typeSystemDocument.parse(schemaStr) match
      case Right((_, schema @ given TypeSystemDocument)) =>
        val A = schema.findTypeDef[ObjectTypeDefinition](Name("A")).get
        validateObjLike(A) match
          case Invalid(errs) =>
            val actualErr   = errs.head
            val expectedErr = InvalidName(Name("__x"))
            assertEquals(clue(actualErr), expectedErr)
          case Valid(_) => fail("Expected to fail")
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
      case Right((_, schema @ given TypeSystemDocument)) =>
        val A = schema.findTypeDef[ObjectTypeDefinition](Name("A")).get
        validateObjLike(A) match
          case Invalid(errs) =>
            val actualErr   = errs.head
            val expectedErr = InvalidType(NamedType(Name("Obj")))
            assertEquals(clue(actualErr), expectedErr)
          case Valid(_) => fail("Expected to fail")
      case _ => fail("failed to parse schemaStr")
  }

  test("An interface must not implement itself") {
    val schemaStr = """
    interface A implements A {
      a: Int!
    }
    """
    typeSystemDocument.parse(schemaStr) match
      case Right((_, schema @ given TypeSystemDocument)) =>
        val A = schema.findTypeDef[InterfaceTypeDefinition](Name("A")).get
        validateObjLike(A) match
          case Invalid(errs) =>
            val actualErr   = errs.head
            val expectedErr = SelfImplementation(NamedType(Name("A")))
            assertEquals(clue(actualErr), expectedErr)
          case Valid(_) => fail("Expected to fail")
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
      case Right((_, schema @ given TypeSystemDocument)) =>
        val C = schema.findTypeDef[UnionTypeDefinition](Name("C")).get
        val D = schema.findTypeDef[UnionTypeDefinition](Name("D")).get

        val noErrs = validateUnion(C)
        assert(noErrs.isValid)

        validateUnion(D) match
          case Invalid(errs) =>
            val actualErr   = errs.head
            val expectedErr = DuplicateName(Name("A"))
            assertEquals(clue(actualErr), expectedErr)
          case Valid(_) => fail("Expected to fail")
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
      case Right((_, schema @ given TypeSystemDocument)) =>
        val planets = schema.findTypeDef[EnumTypeDefinition](Name("Planets")).get
        validateEnum(planets) match
          case Invalid(errs) =>
            val actualErr   = errs.head
            val expectedErr = DuplicateName(Name("MARS"))
            assertEquals(clue(actualErr), expectedErr)
          case Valid(_) => fail("Expected to fail")
      case _ => fail("failed to parse schemaStr")
  }

  test("Input objects must contain one or more fields") {
    val schemaStr = """
    input A {}
    """
    typeSystemDocument.parse(schemaStr) match
      case Right(_ -> schema) => fail("Should not have parsed.")
      case Left(_)            => assert(true)
  }

  test("Input object fields must have valid name") {
    val schemaStr = """
    input A {
      __a: Int
    }
    """
    typeSystemDocument.parse(schemaStr) match
      case Right((_, schema @ given TypeSystemDocument)) =>
        val A = schema.findTypeDef[InputObjectTypeDefinition](Name("A")).get
        validateInputObj(A) match
          case Invalid(errs) =>
            val actualErr   = errs.head
            val expectedErr = InvalidName(Name("__a"))
            assertEquals(clue(actualErr), expectedErr)
          case Valid(_) => fail("Expected to fail")
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
      case Right((_, schema @ given TypeSystemDocument)) =>
        val A = schema.findTypeDef[InputObjectTypeDefinition](Name("A")).get
        val B = schema.findTypeDef[InputObjectTypeDefinition](Name("B")).get

        val noErrs = validateInputObj(A)
        assert(noErrs.isValid)

        validateInputObj(B) match
          case Invalid(errs) =>
            val actualErr   = errs.head
            val expectedErr = InvalidType(NamedType(Name("Obj")))
            assertEquals(clue(actualErr), expectedErr)
          case Valid(_) => fail("Expected to fail")
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
        val errs   = validate(schema).swap.map(_.toList).getOrElse(Nil)
        val actual = errs.filter(_.isInstanceOf[CycleDetected])
        val expected = List(
          CycleDetected(Name("A")),
          CycleDetected(Name("D")),
          CycleDetected(Name("E"))
        )
        assertEquals(clue(actual), clue(expected))
      case _ => fail("failed to parse schemaStr")
  }

  test("directives must have a definition") {
    val doc1 = """
    type A {
      a: Int!
    }
    type B {
      b: String
    }
    union C @mydir = A | B
    """
    typeSystemDocument.parse(doc1) match
      case Right(_ -> schema) =>
        val actualErrs   = validate(schema).swap.map(_.toList).getOrElse(Nil)
        val expectedErrs = List(MissingDefinition(Name("mydir")))
        assertEquals(clue(actualErrs), expectedErrs)
      case _ => fail("failed to parse doc1")
  }

  test("directives must be unique per location") {
    val doc1 = """
    directive @mydir on ENUM

    enum Planets @mydir @mydir {
      MARS
      EARTH
    }
    """
    typeSystemDocument.parse(doc1) match
      case Right(_ -> schema) =>
        val actualErrs   = validate(schema).swap.map(_.toList).getOrElse(Nil)
        val expectedErrs = List(DuplicateName(Name("mydir")))
        assertEquals(clue(actualErrs), expectedErrs)
      case _ => fail("failed to parse doc1")
  }

  test("directives must be in the correct location") {
    val doc1 = """
    enum Planets @deprecated {
      MARS @deprecated
      EARTH
    }
    """
    typeSystemDocument.parse(doc1) match
      case Right(_ -> schema) =>
        val actualErrs   = validate(schema).swap.map(_.toList).getOrElse(Nil)
        val expectedErrs = List(InvalidLocation(Name("deprecated")))
        assertEquals(clue(actualErrs), expectedErrs)
      case _ => fail("failed to parse doc1")
  }

  test("directive definitions must not contain cycles") {
    val schemaStr = """
    type B {
      x: Int @dirB(b: { x: 42 })
    }

    directive @dirA(a: Int @dirA(a: 42)) on ARGUMENT_DEFINITION
    directive @dirB(b: B) on FIELD_DEFINITION
    directive @dirC(b: B) on FIELD_DEFINITION
    """
    typeSystemDocument.parse(schemaStr) match
      case Right(_ -> schema) =>
        val actualErrs =
          validate(schema).swap.map(_.toList).getOrElse(Nil).collect { case o: CycleDetected => o }
        val expectedErrs =
          List[CycleDetected](CycleDetected(Name("dirA")), CycleDetected(Name("dirB")))
        assertEquals(clue(actualErrs), expectedErrs)
      case Left(err) => fail(s"failed to parse schemaStr\n${err}")
  }

  test("directive names must not begin with __") {
    val schemaStr = """
    directive @__dirA on ARGUMENT_DEFINITION
    """
    typeSystemDocument.parse(schemaStr) match
      case Right(_ -> schema) =>
        val actualErrs   = validate(schema).swap.map(_.toList).getOrElse(Nil)
        val expectedErrs = List[GqlError](InvalidName(Name("__dirA")))
        assertEquals(clue(actualErrs), expectedErrs)
      case Left(err) => fail(s"failed to parse schemaStr\n${err}")
  }
end SchemaValidatorSuite

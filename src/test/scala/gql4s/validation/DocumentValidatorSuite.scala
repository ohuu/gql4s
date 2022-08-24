// Copyright (c) 2021 by Oliver Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s
package validation

import cats.implicits.*
import cats.data.NonEmptyList
import munit.FunSuite
import parsing.*
import errors.*

import DocumentValidator.*
import GqlError.*
import Type.*

class DocumentValidationSuite extends FunSuite:
  test("operation names must be unique") {
    // Operation names must be unique
    val doc1Str = """
      query getDogName {
        dog {
          name
        }
      }

      query getOwnerName {
        dog {
          owner {
            name
          }
        }
      }
    """
    executableDocument.parse(doc1Str) match {
      case Right(_ -> doc) => assert(validate(doc).isValid)
      case _               => fail("failed to parse doc1")
    }

    // Operation names must be uniqueu
    val doc2Str = """
      query dogOperation {
        dog {
          name
        }
      }

      mutation dogOperation {
        mutateDog {
          id
        }
      }
    """

    executableDocument.parse(doc2Str) match
      case Right(_ -> doc) =>
        val errs        = validate(doc).swap.map(_.toList).getOrElse(Nil)
        val actualErr   = errs.find(_.isInstanceOf[DuplicateName])
        val expectedErr = DuplicateName(Name("dogOperation"))
        assertEquals(clue(actualErr), clue(Some(expectedErr)))
      case _ => fail("failed to parse doc2")
  }

  test("anonymous operations must be singular") {
    // There can only be one anonymous operation and it must be alone
    val doc1 = """
      {
        dog {
          name
        }
      }
    """
    executableDocument.parse(doc1) match
      case Right(_ -> doc) => assert(validate(doc).isValid)
      case _               => fail("failed to parse doc1")

    val doc2 = """
      {
        dog {
          name
        }
      }

      query getName {
        dog {
          owner {
            name
          }
        }
      }
    """
    executableDocument.parse(doc2) match
      case Right(_ -> doc) =>
        val errs        = validate(doc).swap.map(_.toList).getOrElse(Nil)
        val actualErr   = errs.find(_.isInstanceOf[OperationDefinitionError])
        val expectedErr = OperationDefinitionError(Some("Anonymous operation not alone"))
        assertEquals(clue(actualErr), clue(Some(expectedErr)))
      case _ => fail("failed to parse doc2")
  }

  test("subscriptions should have a single root") {
    val doc1 = """
      subscription sub {
        newMessage {
          body
          sender
        }
      }
    """
    executableDocument.parse(doc1) match
      case Right(_ -> doc) =>
        given ExecutableDocument = doc
        assert(clue(validateSubscriptionsHaveSingleRoot).isValid)
      case _ => fail("failed to parse doc1")

    val doc2 = """
      subscription sub {
        ...newMessageFields
      }

      fragment newMessageFields on Subscription {
        newMessage {
          body
          sender
        }
      }
    """
    executableDocument.parse(doc2) match
      case Right(_ -> doc) =>
        given ExecutableDocument = doc
        assert(validateSubscriptionsHaveSingleRoot.isValid)
      case _ => fail("failed to parse doc2")

    val doc3 = """
      subscription sub {
        newMessage {
          body
          sender
        }
        disallowedSecondRootField
      }
    """
    executableDocument.parse(doc3) match
      case Right(_ -> doc) =>
        given ExecutableDocument = doc
        assert(validateSubscriptionsHaveSingleRoot.isInvalid)
      case _ => fail("failed to parse doc3")

    val doc4 = """
      subscription sub {
        ...multipleSubscriptions
      }

      fragment multipleSubscriptions on Subscription {
        newMessage {
          body
          sender
        }
        disallowedSecondRootField
      }
    """
    executableDocument.parse(doc4) match
      case Right(_ -> doc) =>
        given ExecutableDocument = doc
        assert(validateSubscriptionsHaveSingleRoot.isInvalid)
      case _ => fail("failed to parse doc4")
  }

  test("fields within selection sets must exist") {
    val doc1 = """
      query {
        dog {
          nickname
        }
      }
    """
    executableDocument.parse(doc1) match
      case Right(_ -> doc) => assert(clue(validate(doc)).isValid)
      case _               => fail("failed to parse doc1")

    val doc2 = """
      query {
        dog {
          nickname
          barkVolume
          owner {
            name
            thoughts
          }
        }
      }
    """
    executableDocument.parse(doc2) match
      case Right(_ -> doc) => assert(validate(doc).isValid)
      case _               => fail("failed to parse doc2")

  }

  test("leaf nodes should not have selection sets") {
    val doc1 = """
    fragment scalarSelection on Dog {
      barkVolume
    }

    {
      dog {
        ...scalarSelection
      }
    }
    """
    executableDocument.parse(doc1) match
      case Right(_ -> doc) => assert(clue(validate(doc)).isValid)
      case _               => fail("failed to parse doc1")

    val doc2 = """
    fragment scalarSelectionsNotAllowedOnInt on Dog {
      barkVolume {
        sinceWhen
      }
    }

    {
      ...scalarSelectionsNotAllowedOnInt
    }
    """
    executableDocument.parse(doc2) match
      case Right(_ -> doc) =>
        val errs        = validate(doc).swap.map(_.toList).getOrElse(Nil)
        val actualErr   = errs.find(_.isInstanceOf[InvalidSelection])
        val expectedErr = InvalidSelection(Name("barkVolume"), NamedType(Name("Dog")), None)
        assertEquals(clue(actualErr), clue(Some(expectedErr)))
      case _ => fail("failed to parse doc2")
  }

  test("arguments") {
    val doc1 = """
    fragment argOnRequiredArg on Dog {
      doesKnowCommand(dogCommand: SIT)
    }

    fragment argOnOptional on Dog {
      isHouseTrained(atOtherHomes: true) @include(if: true)
    }

    {
      dog {
        ...argOnRequiredArg
        ...argOnOptional
      }
    }
    """
    executableDocument.parse(doc1) match
      case Right(_ -> doc) => assert(clue(validate(doc)).isValid)
      case _               => fail("failed to parse doc1")

    val doc2 = """
    fragment invalidArgName on Dog {
      doesKnowCommand(command: CLEAN_UP_HOUSE)
    }

    {
      ...invalidArgName
    }
    """
    executableDocument.parse(doc2) match
      case Right(_ -> doc) =>
        val actualErrs = validate(doc).swap.map(_.toList).getOrElse(Nil)
        val expectedErrs = List(
          MissingDefinition(
            Name("command"),
            Some("in definition Name(doesKnowCommand)")
          ),
          MissingArgument2(Name("dogCommand"), None)
        )
        assertEquals(clue(actualErrs), clue(expectedErrs))
      case _ => fail("failed to parse doc2")

    val doc3 = """
    fragment argOnRequiredArg on Dog {
      doesKnowCommand(dogCommand: SIT, dogCommand: SIT)
    }

    {
      ...argOnRequiredArg
    }
    """
    executableDocument.parse(doc3) match
      case Right(_ -> doc) =>
        val errs        = validate(doc).swap.map(_.toList).getOrElse(Nil)
        val actualErr   = errs.find(_.isInstanceOf[DuplicateName])
        val expectedErr = DuplicateName(Name("dogCommand"))
        assertEquals(clue(actualErr), clue(Some(expectedErr)))
      case _ => fail("failed to parse doc3")

    val doc4 = """
    query MyQuery {
      multipleRequirements(x: 1337)
    }
    """
    executableDocument.parse(doc4) match
      case Right(_ -> doc) =>
        val errs        = validate(doc).swap.map(_.toList).getOrElse(Nil)
        val actualErr   = errs.find(_.isInstanceOf[MissingArgument2])
        val expectedErr = MissingArgument2(Name("y"), None)
        assertEquals(clue(actualErr), clue(Some(expectedErr)))
      case _ => fail("failed to parse doc4")
  }

  test("fragment definitions should not be duplicated") {
    val doc1 = """
      {
        dog {
          ...fragmentOne
          ...fragmentTwo
        }
      }

      fragment fragmentOne on Dog {
        name
      }

      fragment fragmentTwo on Dog {
        owner {
          name
        }
      }
    """
    executableDocument.parse(doc1) match
      case Right(_ -> doc) => assert(clue(validate(doc)).isValid)
      case _               => fail("failed to parse doc1")

    val doc2 = """
    {
      dog {
        ...fragmentOne
      }
    }

    fragment fragmentOne on Dog {
      name
    }

    fragment fragmentOne on Dog {
      owner {
        name
      }
    }
    """
    executableDocument.parse(doc2) match
      case Right(_ -> doc) =>
        val errs        = validate(doc).swap.map(_.toList).getOrElse(Nil)
        val actualErr   = errs.find(_.isInstanceOf[DuplicateName])
        val expectedErr = DuplicateName(Name("fragmentOne"))
        assertEquals(clue(actualErr), clue(Some(expectedErr)))
      case _ => fail("failed to parse doc2")
  }

  test("fragments should reference object like types that exist") {
    val doc1 = """
    fragment correctType on Dog {
      name
    }

    fragment inlineFragment on Dog {
      ... on Dog {
        name
      }
    }

    fragment inlineFragment2 on Dog {
      ... @include(if: true) {
        name
      }
    }

    {
      dog {
        ...correctType
      }
      dog {
        ...inlineFragment
      }
      dog {
        ...inlineFragment2
      }
    }
    """
    executableDocument.parse(doc1) match
      case Right(_ -> doc) => assert(clue(validate(doc)).isValid)
      case _               => fail("failed to parse doc1")

    val doc2 = """
    fragment notOnExistingType on NotInSchema {
      name
    }

    fragment inlineNotExistingType on Dog {
      ... on NotInSchema {
        name
      }
    }

    {
      ...notOnExistingType

      dog {
        ...inlineNotExistingType
      }
    }
    """
    executableDocument.parse(doc2) match
      case Right(_ -> doc) =>
        val actualErrs = validate(doc).swap.map(_.toList).getOrElse(Nil)
        val expectedErrs = List(
          MissingField2(Name("name"), NamedType(Name("NotInSchema")), None),
          MissingDefinition(Name("NotInSchema"), None),
          InvalidFragment(Name("NotInSchema"))
        )
        assertEquals(clue(actualErrs), clue(expectedErrs))
      case _ => fail("failed to parse doc1")
  }

  test("fragments must be used") {
    val doc1 = """
    fragment nameFragment on Dog { # unused
      name
    }

    {
      dog {
        name
      }
    }
    """
    executableDocument.parse(doc1) match
      case Right(_ -> doc) =>
        val errs        = validate(doc).swap.map(_.toList).getOrElse(Nil)
        val actualErr   = errs.find(_.isInstanceOf[UnusedDefinition])
        val expectedErr = UnusedDefinition(Name("nameFragment"))
        assertEquals(clue(actualErr), clue(Some(expectedErr)))
      case _ => fail("failed to parse doc1")
  }

  test("fragment spread must refer to a fragment definition that exists") {
    val doc = """
    {
      dog {
        ...undefinedFragment
      }
    }
    """
    executableDocument.parse(doc) match
      case Right(_ -> doc) =>
        val errs     = validate(doc).swap.map(_.toList).getOrElse(Nil)
        val actual   = errs.find(_.isInstanceOf[MissingDefinition])
        val expected = MissingDefinition(Name("undefinedFragment"))
        assertEquals(clue(actual), clue(Some(expected)))
      case _ => fail("failed to parse doc")
  }

  test("fragment definitions must not contain cycles") {
    val doc1 = """
    {
      dog {
        ...nameFragment
        ...barkVolumeFragment
      }
    }

    fragment nameFragment on Dog {
      name
      ...barkVolumeFragment
    }

    fragment barkVolumeFragment on Dog {
      barkVolume
      ...nameFragment
    }
    """
    executableDocument.parse(doc1) match
      case Right(_ -> doc) =>
        val errs   = validate(doc).swap.map(_.toList).getOrElse(Nil)
        val actual = errs.filter(_.isInstanceOf[CycleDetected])
        val expected = List(
          CycleDetected(Name("nameFragment")),
          CycleDetected(Name("barkVolumeFragment"))
        )
        assertEquals(clue(actual), clue(expected))
      case _ => fail("failed to parse doc")
  }

  test("input objects fields must be defined") {
    val doc1 = """
    {
      findDog(complex: { name: { first: "Turner" }, owner: "Fido" }) {
        name
      }
    }
    """

    val doc2 = """
    {
      findDog(complex: { name: { first: "fido" } }) {
        name
      }
    }
    """

    val doc3 = """
    {
      findDog(complex: { favoriteCookieFlavor: "Bacon" }) {
        name
      }
    }
    """

    executableDocument.parse(doc1) match
      case Right(_ -> doc) => assert(clue(validate(doc)).isValid)
      case _               => fail("failed to parse doc1")

    executableDocument.parse(doc2) match
      case Right(_ -> doc) => assert(clue(validate(doc)).isValid)
      case _               => fail("failed to parse doc2")

    executableDocument.parse(doc3) match
      case Right(_ -> doc) =>
        val errs   = validate(doc).swap.map(_.toList).getOrElse(Nil)
        val actual = errs.filter(_.isInstanceOf[MissingField2])
        val expected = List(
          MissingField2(Name("favoriteCookieFlavor"), NamedType(Name("ComplexInput"))),
          MissingField2(Name("name"), NamedType(Name("ComplexInput")))
        )
        assertEquals(clue(actual), clue(expected))
      case _ => fail("failed to parse doc3")
  }

  test("input object fields must be unique") {
    val doc1 = """
    {
      findDog(complex: { name: { first: "fido" }, name: { first: "asdf" } }) {
        name
      }
    }
    """

    val doc2 = """
    {
      findDog(complex: { name: { first: "fido", first: "asdf" } }) {
        name
      }
    }
    """

    executableDocument.parse(doc1) match
      case Right(_ -> doc) =>
        val errs     = validate(doc).swap.map(_.toList).getOrElse(Nil)
        val actual   = errs.find(_.isInstanceOf[DuplicateName])
        val expected = DuplicateName(Name("name"))
        assertEquals(clue(actual), clue(Some(expected)))
      case _ => fail("failed to parse doc1")

    executableDocument.parse(doc2) match
      case Right(_ -> doc) =>
        val errs     = validate(doc).swap.map(_.toList).getOrElse(Nil)
        val actual   = errs.find(_.isInstanceOf[DuplicateName])
        val expected = DuplicateName(Name("first"))
        assertEquals(clue(actual), clue(Some(expected)))
      case _ => fail("failed to parse doc2")
  }

  test("input object values must include required fields") {
    val doc1 = """
    {
      findDog(complex: { owner: "Turner" }) {
        name
      }
    }
    """

    executableDocument.parse(doc1) match
      case Right(_ -> doc) =>
        val errs     = validate(doc).swap.map(_.toList).getOrElse(Nil)
        val actual   = errs.find(_.isInstanceOf[MissingField2])
        val expected = MissingField2(Name("name"), NamedType(Name("ComplexInput")))
        assertEquals(clue(actual), clue(Some(expected)))
      case _ => fail("failed to parse doc1")
  }

  test("variables must be unique") {
    val doc1 = """
    query houseTrainedQuery($atOtherHomes: Boolean, $atOtherHomes: Boolean) {
      dog {
        isHouseTrained(atOtherHomes: $atOtherHomes)
      }
    }
    """
    executableDocument.parse(doc1) match
      case Right(_ -> doc) =>
        val errs     = validate(doc).swap.map(_.toList).getOrElse(Nil)
        val actual   = errs.find(_.isInstanceOf[DuplicateName])
        val expected = DuplicateName(Name("atOtherHomes"))
        assertEquals(clue(actual), clue(Some(expected)))
      case _ => fail("failed to parse doc1")

    val doc2 = """
    query A($atOtherHomes: Boolean) {
      ...HouseTrainedFragment
    }

    query B($atOtherHomes: Boolean) {
      ...HouseTrainedFragment
    }

    fragment HouseTrainedFragment on Query {
      dog {
        isHouseTrained(atOtherHomes: $atOtherHomes)
      }
    }
    """
    executableDocument.parse(doc2) match
      case Right(_ -> doc) => assert(validate(doc).isValid)
      case _               => fail("failed to parse doc2")
  }

  test("variables must be an input type") {
    val doc1 = """
    query takesBoolean($atOtherHomes: Boolean) {
      dog {
        isHouseTrained(atOtherHomes: $atOtherHomes)
      }
    }

    query takesComplexInput($complexInput: ComplexInput) {
      findDog(complex: $complexInput) {
        name
      }
    }

    query TakesListOfBooleanBang($booleans: [Boolean!]) {
      booleanList(booleanListArg: $booleans)
    }
    """
    executableDocument.parse(doc1) match
      case Right(_ -> doc) => assert(validate(doc).isValid)
      case _               => fail("failed to parse doc1")

    val doc2 = """
    query takesCat($cat: Cat) {
      name
    }

    query takesDogBang($dog: Dog!) {
      name
    }

    query takesListOfPet($pets: [Pet]) {
      name
    }

    query takesCatOrDog($catOrDog: CatOrDog) {
      name
    }
    """

    executableDocument.parse(doc2) match
      case Right(_ -> doc) =>
        val errs   = validate(doc).swap.map(_.toList).getOrElse(Nil)
        val actual = errs.filter(_.isInstanceOf[InvalidType])
        val expected =
          List(
            InvalidType(NamedType(Name("Cat"))),
            InvalidType(NonNullType(NamedType(Name("Dog")))),
            InvalidType(ListType(NamedType(Name("Pet")))),
            InvalidType(NamedType(Name("CatOrDog")))
          )
        assertEquals(clue(actual), clue(expected))
      case _ => fail("failed to parse doc2")
  }

  test("variable references must be defined") {
    val doc1 = """
    query variableIsDefined($atOtherHomes: Boolean) {
      dog {
        isHouseTrained(atOtherHomes: $atOtherHomes)
      }
    }
    """
    executableDocument.parse(doc1) match
      case Right(_ -> doc) => assert(clue(validate(doc)).isValid)
      case _               => fail("failed to parse doc1")

    val doc2 = """
    query variableIsNotDefined {
      dog {
          isHouseTrained(atOtherHomes: $atOtherHomes)
      }
    }
    """
    executableDocument.parse(doc2) match
      case Right(_ -> doc) =>
        val errs     = validate(doc).swap.map(_.toList).getOrElse(Nil)
        val actual   = errs.filter(_.isInstanceOf[MissingVariable2])
        val expected = List(MissingVariable2(Name("atOtherHomes")))
        assertEquals(clue(actual), clue(expected))
      case _ => fail("failed to parse doc2")

    val doc3 = """
    fragment isHouseTrainedFragment on Dog {
      isHouseTrained(atOtherHomes: $atOtherHomes)
    }

    query variableIsNotDefined {
      dog {
        ...isHouseTrainedFragment
      }
    }
    """
    executableDocument.parse(doc3) match
      case Right(_ -> doc) =>
        val errs     = validate(doc).swap.map(_.toList).getOrElse(Nil)
        val actual   = errs.filter(_.isInstanceOf[MissingVariable2])
        val expected = List(MissingVariable2(Name("atOtherHomes")))
        assertEquals(clue(actual), clue(expected))
      case _ => fail("failed to parse doc3")
  }

  test("all variables must be used") {
    val doc1 = """
    query variableUnused($atOtherHomes: Boolean) {
      dog {
        isHouseTrained
      }
    }
    """
    executableDocument.parse(doc1) match
      case Right(_ -> doc) =>
        val errs     = validate(doc).swap.map(_.toList).getOrElse(Nil)
        val actual   = errs.filter(_.isInstanceOf[UnusedDefinition])
        val expected = List(UnusedDefinition(Name("atOtherHomes")))
        assertEquals(clue(actual), clue(expected))
      case _ => fail("failed to parse doc1")
  }

  test("fragment spread be assignable to the parent type") {
    val doc1 = """
    fragment dogFragment on Dog {
      ... on Dog {
        barkVolume
      }
    }

    {
      dog {
        ...dogFragment
      }
    }
    """
    executableDocument.parse(doc1) match
      case Right(_ -> doc) =>
        val errs = validate(doc)
        assert(clue(errs.isValid))
      case _ => fail("failed to parse doc1")

    val doc2 = """
    fragment catInDogFragmentInvalid on Dog {
      ... on Cat {
        meowVolume
      }
    }

    {
      dog {
        ...catInDogFragmentInvalid
      }
    }
    """
    executableDocument.parse(doc2) match
      case Right(_ -> doc) =>
        val actualErrs   = validate(doc).swap.map(_.toList).getOrElse(Nil)
        val expectedErrs = List(InvalidFragment(Name("Cat")))
        assertEquals(clue(actualErrs), clue(expectedErrs))
      case _ => fail("failed to parse doc2")

    val doc3 = """
    fragment petNameFragment on Pet {
      name
    }

    fragment interfaceWithinObjectFragment on Dog {
      ...petNameFragment
    }

    {
      dog {
        ...interfaceWithinObjectFragment
      }
    }
    """
    executableDocument.parse(doc3) match
      case Right(_ -> doc) => assert(validate(doc).isValid)
      case _               => fail("failed to parse doc3")

    val doc4 = """
    fragment catOrDogNameFragment on CatOrDog {
      ... on Cat {
        meowVolume
      }
    }

    fragment unionWithObjectFragment on Dog {
      ...catOrDogNameFragment
    }

    {
      dog {
        ...unionWithObjectFragment
      }
    }
    """
    executableDocument.parse(doc4) match
      case Right(_ -> doc) => assert(validate(doc).isValid)
      case _               => fail("failed to parse doc4")

    val doc5 = """
    fragment petFragment on Pet {
      name
      ... on Dog {
        barkVolume
      }
    }

    fragment catOrDogFragment on CatOrDog {
      ... on Cat {
        meowVolume
      }
    }

    {
      dog {
        ...petFragment
        ...catOrDogFragment
      }
    }
    """
    executableDocument.parse(doc5) match
      case Right(_ -> doc) => assert(validate(doc).isValid)
      case _               => fail("failed to parse doc5")

    val doc6 = """
    fragment sentientFragment on Sentient {
      ... on Dog {
        barkVolume
      }
    }

    fragment humanOrAlienFragment on HumanOrAlien {
      ... on Cat {
        meowVolume
      }
    }

    {
      dog {
        owner {
          ...humanOrAlienFragment
          ...sentientFragment
        }
      }
    }
    """
    executableDocument.parse(doc6) match
      case Right(_ -> doc) => assert(validate(doc).isInvalid)
      case _               => fail("failed to parse doc6")
  }

  test("directives must have a definition") {
    val doc1 = """
    {
      dog {
        name @skip
        barkVolume @skips
      }
    }
    """
    executableDocument.parse(doc1) match
      case Right(_ -> doc) =>
        val actualErrs   = validate(doc).swap.map(_.toList).getOrElse(Nil)
        val expectedErrs = List(MissingDefinition(Name("skips")))
        assertEquals(clue(actualErrs), expectedErrs)
      case _ => fail("failed to parse doc1")
  }

  test("directives must be unique per location") {
    val doc1 = """
    {
      dog {
        name @skip @skip
      }
    }
    """
    executableDocument.parse(doc1) match
      case Right(_ -> doc) =>
        val actualErrs   = validate(doc).swap.map(_.toList).getOrElse(Nil)
        val expectedErrs = List(DuplicateName(Name("skip")))
        assertEquals(clue(actualErrs), expectedErrs)
      case _ => fail("failed to parse doc1")

  }

  test("directives must be in the correct location") {
    val doc1 = """
    {
      dog {
        name @deprecated
      }
    }
    """
    executableDocument.parse(doc1) match
      case Right(_ -> doc) =>
        val actualErrs   = validate(doc).swap.map(_.toList).getOrElse(Nil)
        val expectedErrs = List(InvalidLocation(Name("deprecated")))
        assertEquals(clue(actualErrs), expectedErrs)
      case _ => fail("failed to parse doc1")
  }
end DocumentValidationSuite

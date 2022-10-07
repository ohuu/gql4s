// Copyright (c) 2021 by Oliver Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s
package validation

import cats.data.{NonEmptyChain as NEC, Chain}
import cats.data.Validated.{Invalid, Valid}
import cats.implicits.*

import validation.DocumentValidator.*
import parsing.ExecutableDirectiveLocation as EDL
import errors.GqlError.*
import munit.FunSuite
import parsing.*
import parsing.Type.*

class DocumentValidationSuite extends FunSuite:
    given schemaCtx: SchemaContext = SchemaContext(schemaDoc)

    test("operation names must be unique"):
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
        executableDocument.parse(doc1Str) match
            case Right(_ -> doc) => assert(validate(doc, schemaCtx).isValid)
            case _               => fail("failed to parse doc1")

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
                validate(doc, schemaCtx) match
                    case Invalid(actualErrs) =>
                        val expectedErrs = NEC(
                            DuplicateOperationDefinitions(List(Name("dogOperation"))),
                            OperationTypeMissingTypeDefinition(OperationType.Mutation)
                        )
                        assertEquals(clue(actualErrs), expectedErrs)
                    case Valid(_) => fail("expected to fail")
            case _ => fail("failed to parse doc2")

    test("anonymous operations must be singular"):
        // There can only be one anonymous operation and it must be alone
        val doc1 = """
        {
            dog {
                name
            }
        }
        """
        executableDocument.parse(doc1) match
            case Right(_ -> doc) => assert(validate(doc, schemaCtx).isValid)
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
                validate(doc, schemaCtx) match
                    case Invalid(actualErrs) =>
                        val expectedErrs = NEC(AnonOperationDefinitionNotAlone)
                        assertEquals(clue(actualErrs), expectedErrs)
                    case Valid(_) => fail("expected to fail")
            case _ => fail("failed to parse doc2")

    test("subscriptions should have a single root"):
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
                given Context = Context(schemaCtx, DocumentContext(doc))
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
                given Context = Context(schemaCtx, DocumentContext(doc))
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
                given Context = Context(schemaCtx, DocumentContext(doc))
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
                given Context = Context(schemaCtx, DocumentContext(doc))
                assert(validateSubscriptionsHaveSingleRoot.isInvalid)
            case _ => fail("failed to parse doc4")

    test("fields within selection sets must exist"):
        val doc1 = """
        query {
            dog {
                nickname
            }
        }
        """
        executableDocument.parse(doc1) match
            case Right(_ -> doc) => assert(clue(validate(doc, schemaCtx)).isValid)
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
            case Right(_ -> doc) => assert(validate(doc, schemaCtx).isValid)
            case _               => fail("failed to parse doc2")

    test("leaf nodes should not have selection sets"):
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
            case Right(_ -> doc) => assert(clue(validate(doc, schemaCtx)).isValid)
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
                validate(doc, schemaCtx) match
                    case Invalid(actualErrs) =>
                        val expectedErrs = NEC(SelectionSetOnNonObjectLikeType(Name("barkVolume"), NamedType(Name("Dog"))))
                        assertEquals(clue(actualErrs), expectedErrs)
                    case Valid(_) => fail("expected to fail")
            case _ => fail("failed to parse doc2")

    test("arguments must be defined"):
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
            case Right(_ -> doc) => assert(clue(validate(doc, schemaCtx)).isValid)
            case _               => fail("failed to parse doc1")

        val doc2 = """
        fragment invalidArgName on Dog {
            isHouseTrained(atOtherHome: true)
        }

        {
            dog {
                ...invalidArgName
            }
        }
        """
        executableDocument.parse(doc2) match
            case Right(_ -> doc) =>
                validate(doc, schemaCtx) match
                    case Invalid(actualErrs) =>
                        val expectedErrs = NEC(InputValueDefinitionMissing(Name("atOtherHome")))
                        assertEquals(clue(actualErrs), expectedErrs)
                    case Valid(_) => fail("expected to fail")
            case _ => fail("failed to parse doc2")

    test("required arguments must be provided"):
        val doc1 = """
        fragment invalidArgName on Dog {
            doesKnowCommand(command: CLEAN_UP_HOUSE)
        }

        {
            dog {
                ...invalidArgName
            }
        }
        """
        executableDocument.parse(doc1) match
            case Right(_ -> doc) =>
                validate(doc, schemaCtx) match
                    case Invalid(actualErrs) =>
                        val expectedErrs = NEC(
                            InputValueDefinitionMissing(Name("command")),
                            RequiredArgumentMissing(Name("dogCommand"), Name("doesKnowCommand"))
                        )
                        assertEquals(clue(actualErrs), expectedErrs)
                    case Valid(_) => fail("expected to fail")
            case _ => fail("failed to parse doc2")

        val doc2 = """
        query MyQuery {
            multipleRequirements(x: 1337)
        }
        """
        executableDocument.parse(doc2) match
            case Right(_ -> doc) =>
                validate(doc, schemaCtx) match
                    case Invalid(actualErrs) =>
                        val expectedErrs = NEC(
                            RequiredArgumentMissing(Name("y"), Name("multipleRequirements"))
                        )
                        assertEquals(clue(actualErrs), expectedErrs)
                    case Valid(_) => fail("expected to fail")
            case _ => fail("failed to parse doc4")

    test("arguments must not be duplicated"):
        val doc1 = """
        fragment argOnRequiredArg on Dog {
            doesKnowCommand(dogCommand: SIT, dogCommand: SIT)
        }

        {
            dog {
                ...argOnRequiredArg
            }
        }
        """
        executableDocument.parse(doc1) match
            case Right(_ -> doc) =>
                validate(doc, schemaCtx) match
                    case Invalid(actualErrs) =>
                        val expectedErrs = NEC(DuplicateArguments(List(Name("dogCommand"))))
                        assertEquals(clue(actualErrs), expectedErrs)
                    case Valid(_) => fail("expected to fail")
            case _ => fail("failed to parse doc3")

    test("fragment definitions should not be duplicated"):
        val doc1 = """
        fragment fragmentOne on Dog {
            name
        }

        fragment fragmentTwo on Dog {
            owner {
                name
            }
        }

        {
            dog {
                ...fragmentOne
                ...fragmentTwo
            }
        }
        """
        executableDocument.parse(doc1) match
            case Right(_ -> doc) => assert(clue(validate(doc, schemaCtx)).isValid)
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
                validate(doc, schemaCtx) match
                    case Invalid(actualErrs) =>
                        val expectedErrs = NEC(DuplicateFragmentDefinitions(List(Name("fragmentOne"))))
                        assertEquals(clue(actualErrs), expectedErrs)
                    case Valid(_) => fail("expected to fail")
            case _ => fail("failed to parse doc2")

    test("fragments should reference object like types that exist"):
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
            case Right(_ -> doc) => assert(clue(validate(doc, schemaCtx)).isValid)
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
                validate(doc, schemaCtx) match
                    case Invalid(actualErrs) =>
                        val expectedErrs = NEC(
                            FieldDefinitionMissing(Name("name"), Name("NotInSchema")),
                            FragmentDefinitionHasMissingType(Name("NotInSchema")),
                            FragmentUsedOnWrongType(Name("Dog"), Name("NotInSchema"))
                        )
                        assertEquals(clue(actualErrs), expectedErrs)
                    case Valid(_) => fail("expected to fail")
            case _ => fail("failed to parse doc1")

    test("fragments must be used"):
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
                validate(doc, schemaCtx) match
                    case Invalid(actualErrs) =>
                        val expectedErrs = NEC(FragmentDefinitionUnused(Name("nameFragment")))
                        assertEquals(clue(actualErrs), expectedErrs)
                    case Valid(_) => fail("expected to fail")
            case _ => fail("failed to parse doc1")

    test("fragment spread must refer to a fragment definition that exists"):
        val doc = """
        {
            dog {
                ...undefinedFragment
            }
        }
        """
        executableDocument.parse(doc) match
            case Right(_ -> doc) =>
                validate(doc, schemaCtx) match
                    case Invalid(actualErrs) =>
                        val expectedErrs = NEC(FragmentDefinitionMissing(Name("undefinedFragment")))
                        assertEquals(clue(actualErrs), expectedErrs)
                    case Valid(_) => fail("expected to fail")
            case _ => fail("failed to parse doc")

    test("fragment definitions must not contain cycles"):
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
                validate(doc, schemaCtx) match
                    case Invalid(actualErrs) =>
                        val expectedErrs = NEC(
                            FragmentDefinitionHasCyclicalDependency(
                                List(
                                    Name("nameFragment"),
                                    Name("barkVolumeFragment")
                                )
                            )
                        )
                        assertEquals(clue(actualErrs), expectedErrs)
                    case Valid(_) => fail("expected to fail")
            case _ => fail("failed to parse doc")

    test("input objects fields must be defined"):
        val doc1 = """
        {
            findDog(complex: { name: { first: "Turner" }, owner: "Fido" }) {
                name
            }
        }
        """
        executableDocument.parse(doc1) match
            case Right(_ -> doc) => assert(clue(validate(doc, schemaCtx)).isValid)
            case _               => fail("failed to parse doc1")

        val doc2 = """
        {
            findDog(complex: { name: { first: "fido" } }) {
                name
            }
        }
        """
        executableDocument.parse(doc2) match
            case Right(_ -> doc) => assert(clue(validate(doc, schemaCtx)).isValid)
            case _               => fail("failed to parse doc2")

        val doc3 = """
        {
            findDog(complex: { favoriteCookieFlavor: "Bacon" }) {
                name
            }
        }
        """
        executableDocument.parse(doc3) match
            case Right(_ -> doc) =>
                validate(doc, schemaCtx) match
                    case Invalid(actualErrs) =>
                        val expectedErrs = NEC(
                            InputObjectFieldMissing(Name("favoriteCookieFlavor"), Name("ComplexInput")),
                            InputObjectFieldRequired(Name("name"), Name("ComplexInput"))
                        )
                        assertEquals(clue(actualErrs), expectedErrs)
                    case Valid(_) => fail("expected to fail")
            case _ => fail("failed to parse doc3")

    test("input object fields must be unique"):
        val doc1 = """
        {
            findDog(complex: { name: { first: "fido" }, name: { first: "asdf" } }) {
                name
            }
        }
        """
        executableDocument.parse(doc1) match
            case Right(_ -> doc) =>
                validate(doc, schemaCtx) match
                    case Invalid(actualErrs) =>
                        val expectedErrs = NEC(DuplicateFields(List(Name("name"))))
                        assertEquals(clue(actualErrs), expectedErrs)
                    case Valid(_) => fail("expected to fail")
            case _ => fail("failed to parse doc1")

        val doc2 = """
        {
            findDog(complex: { name: { first: "fido", first: "asdf" } }) {
                name
            }
        }
        """
        executableDocument.parse(doc2) match
            case Right(_ -> doc) =>
                validate(doc, schemaCtx) match
                    case Invalid(actualErrs) =>
                        val expectedErrs = NEC(DuplicateFields(List(Name("first"))))
                        assertEquals(clue(actualErrs), expectedErrs)
                    case Valid(_) => fail("expected to fail")
            case _ => fail("failed to parse doc2")

    test("input object values must include required fields"):
        val doc1 = """
        {
            findDog(complex: { owner: "Turner" }) {
                name
            }
        }
        """
        executableDocument.parse(doc1) match
            case Right(_ -> doc) =>
                validate(doc, schemaCtx) match
                    case Invalid(actualErrs) =>
                        val expectedErrs = NEC(InputObjectFieldRequired(Name("name"), Name("ComplexInput")))
                        assertEquals(clue(actualErrs), expectedErrs)
                    case Valid(_) => fail("expected to fail")
            case _ => fail("failed to parse doc1")

    test("variables must be unique"):
        val doc1 = """
        query houseTrainedQuery($atOtherHomes: Boolean, $atOtherHomes: Boolean) {
            dog {
                isHouseTrained(atOtherHomes: $atOtherHomes)
            }
        }
        """
        executableDocument.parse(doc1) match
            case Right(_ -> doc) =>
                validate(doc, schemaCtx) match
                    case Invalid(actualErrs) =>
                        val expectedErrs = NEC(DuplicateVariables(List(Name("atOtherHomes"))))
                        assertEquals(clue(actualErrs), expectedErrs)
                    case Valid(_) => fail("expected to fail")
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
            case Right(_ -> doc) => assert(clue(validate(doc, schemaCtx).isValid))
            case _               => fail("failed to parse doc2")

    test("variables must be an input type"):
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
            case Right(_ -> doc) => assert(clue(validate(doc, schemaCtx).isValid))
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
                validate(doc, schemaCtx) match
                    case Invalid(actualErrs) =>
                        val expectedErrs =
                            NEC(
                                VariableUnused(Name("cat")),
                                NonInputType(NamedType(Name("Cat"))),
                                VariableUnused(Name("dog")),
                                NonInputType(NonNullType(NamedType(Name("Dog")))),
                                VariableUnused(Name("pets")),
                                NonInputType(ListType(NamedType(Name("Pet")))),
                                VariableUnused(Name("catOrDog")),
                                NonInputType(NamedType(Name("CatOrDog")))
                            )
                        assertEquals(clue(actualErrs), expectedErrs)
                    case Valid(_) => fail("expected to fail")
            case _ => fail("failed to parse doc2")

    test("variable references must be defined"):
        val doc1 = """
        query variableIsDefined($atOtherHomes: Boolean) {
            dog {
                isHouseTrained(atOtherHomes: $atOtherHomes)
            }
        }
        """
        executableDocument.parse(doc1) match
            case Right(_ -> doc) => assert(clue(validate(doc, schemaCtx)).isValid)
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
                validate(doc, schemaCtx) match
                    case Invalid(actualErrs) =>
                        val expectedErrs = NEC(VariableDefinitionMissing(Name("atOtherHomes")))
                        assertEquals(clue(actualErrs), expectedErrs)
                    case Valid(_) => fail("expected to fail")
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
                validate(doc, schemaCtx) match
                    case Invalid(actualErrs) =>
                        val expectedErrs = NEC(VariableDefinitionMissing(Name("atOtherHomes")))
                        assertEquals(clue(actualErrs), expectedErrs)
                    case Valid(_) => fail("expected to fail")
            case _ => fail("failed to parse doc3")

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
                validate(doc, schemaCtx) match
                    case Invalid(actualErrs) =>
                        val expectedErrs = NEC(VariableUnused(Name("atOtherHomes")))
                        assertEquals(clue(actualErrs), expectedErrs)
                    case Valid(_) => fail("expected to fail")
            case _ => fail("failed to parse doc1")
    }

    test("fragment spread be assignable to the parent type"):
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
            case Right(_ -> doc) => assert(clue(validate(doc, schemaCtx).isValid))
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
                validate(doc, schemaCtx) match
                    case Invalid(actualErrs) =>
                        val expectedErrs = NEC(FragmentUsedOnWrongType(Name("Dog"), Name("Cat")))
                        assertEquals(clue(actualErrs), expectedErrs)
                    case Valid(_) => fail("expected to fail")
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
            case Right(_ -> doc) => assert(validate(doc, schemaCtx).isValid)
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
            case Right(_ -> doc) => assert(validate(doc, schemaCtx).isValid)
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
            case Right(_ -> doc) => assert(validate(doc, schemaCtx).isValid)
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
            case Right(_ -> doc) => assert(validate(doc, schemaCtx).isInvalid)
            case _               => fail("failed to parse doc6")

    test("directives must have a definition"):
        val doc1 = """
        query myQuery($someTest: Boolean!) {
            dog {
                name @skip(if: $someTest)
                barkVolume @skips
            }
        }
        """
        executableDocument.parse(doc1) match
            case Right(_ -> doc) =>
                validate(doc, schemaCtx) match
                    case Invalid(actualErrs) =>
                        val expectedErrs = NEC(DirectiveDefinitionMissing(Name("skips")))
                        assertEquals(clue(actualErrs), expectedErrs)
                    case Valid(_) => fail("expected to fail")
            case _ => fail("failed to parse doc1")

    test("directives must be unique per location"):
        val doc1 = """
        query myQuery($someTest: Boolean!) {
            dog {
                name @skip(if: $someTest) @skip(if: $someTest)
            }
        }
        """
        executableDocument.parse(doc1) match
            case Right(_ -> doc) =>
                validate(doc, schemaCtx) match
                    case Invalid(actualErrs) =>
                        val expectedErrs = NEC(DuplicateDirectives(List(Name("skip"))))
                        assertEquals(clue(actualErrs), expectedErrs)
                    case Valid(_) => fail("expected to fail")
            case _ => fail("failed to parse doc1")

    test("directives must be in the correct location"):
        val doc1 = """
        {
            dog {
                name @deprecated
            }
        }
        """
        executableDocument.parse(doc1) match
            case Right(_ -> doc) =>
                validate(doc, schemaCtx) match
                    case Invalid(actualErrs) =>
                        val expectedErrs = NEC(IllegalDirectiveLocation(Name("deprecated"), EDL.FIELD))
                        assertEquals(clue(actualErrs), expectedErrs)
                    case Valid(_) => fail("expected to fail")
            case _ => fail("failed to parse doc1")

    test("directive arguments must be defined and unique and requied args must be provided"):
        val doc1 = """
        query GetDogA {
            dog {
                name @myDir(x: 42, y: 42)
            }
        }

        query GetDogB {
            dog {
                name @myDir(x: 42, x: 42)
            }
        }

        query GetDogC {
            dog {
                name @myDir
            }
        }
        """
        executableDocument.parse(doc1) match
            case Right(_ -> doc) =>
                validate(doc, schemaCtx) match
                    case Invalid(actualErrs) =>
                        val expectedErrs = NEC(
                            InputValueDefinitionMissing(Name("y")),
                            DuplicateArguments(List(Name("x"))),
                            RequiredArgumentMissing(Name("x"), Name("myDir"))
                        )
                        assertEquals(clue(actualErrs), expectedErrs)
                    case Valid(_) => fail("expected to fail")
            case _ => fail("failed to parse doc1")

    test("scalar values must be the correct type"):
        val doc1 = """
        fragment DogFragment on Dog {
            isHouseTrained(atOtherHomes: $houseTrained)
        }

        query TestQuery($houseTrained: Boolean) {
            testInt(x: 42)
            testFloat(x: 42.0)
            testBoolean(x: true)
            findDog(complex: { name: { first: "Oliver", last: "Winks" }, owner: "Me" }) {
                ...DogFragment
            }
        }
        """
        executableDocument.parse(doc1) match
            case Right(_ -> doc) => assert(validate(doc, schemaCtx).isValid)
            case _ => fail("failed to parse doc1")

        val doc2 = """
        query DoThing($xx: Boolean) {
            testBoolean(x: $xx)
        }
        """
        executableDocument.parse(doc2) match
            case Right(_ -> doc) => assert(validate(doc, schemaCtx).isValid)
            case _ => fail("failed to parse doc1")
end DocumentValidationSuite

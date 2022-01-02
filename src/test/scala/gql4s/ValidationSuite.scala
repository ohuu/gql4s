// Copyright (c) 2021 by Oliver Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s

import cats.data.NonEmptyList
import munit.FunSuite

// TODO: A lot of these tests for test for the existence of Left or Right. We should change them
//       to check for actual error types e.g. Left(List(DuplicateFragmentDefinition...))
class ValidationSuite extends FunSuite:
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
      case Right(_ -> doc) => assert(validate(doc, schemaDoc).isRight)
      case _               => fail("failed to parse doc1")
    }

    // Operation names must be unique
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
      case Right(_ -> doc) => assert(validate(doc, schemaDoc).isLeft)
      case _               => fail("failed to parse doc2")
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
      case Right(_ -> doc) => assert(validate(doc, schemaDoc).isRight)
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
      case Right(_ -> doc) => assert(validate(doc, schemaDoc).isLeft)
      case _               => fail("failed to parse doc2")
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
      case Right(_ -> doc) => assert(clue(subscriptionsHaveSingleRoot(doc)).isEmpty)
      case _               => fail("failed to parse doc1")

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
      case Right(_ -> doc) => assert(subscriptionsHaveSingleRoot(doc).isEmpty)
      case _               => fail("failed to parse doc2")

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
      case Right(_ -> doc) => assert(subscriptionsHaveSingleRoot(doc).length > 0)
      case _               => fail("failed to parse doc3")

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
      case Right(_ -> doc) => assert(subscriptionsHaveSingleRoot(doc).length > 0)
      case _               => fail("failed to parse doc4")
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
      case Right(_ -> doc) => assert(clue(validate(doc, schemaDoc)).isRight)
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
      case Right(_ -> doc) => assert(validate(doc, schemaDoc).isRight)
      case _               => fail("failed to parse doc2")

  }

  test("leaf nodes should not have selection sets") {
    val doc1 = """
    fragment scalarSelection on Dog {
      barkVolume
    }
    """
    executableDocument.parse(doc1) match
      case Right(_ -> doc) => assert(clue(validate(doc, schemaDoc)).isRight)
      case _               => fail("failed to parse doc1")

    val doc2 = """
    fragment scalarSelectionsNotAllowedOnInt on Dog {
      barkVolume {
        sinceWhen
      }
    }
    """
    executableDocument.parse(doc2) match
      case Right(_ -> doc) => assert(clue(validate(doc, schemaDoc)).isLeft)
      case _               => fail("failed to parse doc2")
  }

  test("arguments") {
    val doc1 = """
    fragment argOnRequiredArg on Dog {
      doesKnowCommand(dogCommand: SIT)
    }

    fragment argOnOptional on Dog {
      isHouseTrained(atOtherHomes: true) @include(if: true)
    }
    """
    executableDocument.parse(doc1) match
      case Right(_ -> doc) => assert(clue(validate(doc, schemaDoc)).isRight)
      case _               => fail("failed to parse doc1")

    val doc2 = """
    fragment invalidArgName on Dog {
      doesKnowCommand(command: CLEAN_UP_HOUSE)
    }
    """
    executableDocument.parse(doc2) match
      case Right(_ -> doc) => assert(clue(validate(doc, schemaDoc)).isLeft)
      case _               => fail("failed to parse doc2")

    val doc3 = """
    fragment argOnRequiredArg on Dog {
      doesKnowCommand(dogCommand: SIT, dogCommand: SIT)
    }
    """
    executableDocument.parse(doc3) match
      case Right(_ -> doc) => assert(clue(validate(doc, schemaDoc)).isLeft)
      case _               => fail("failed to parse doc3")

    val doc4 = """
    query MyQuery {
      multipleRequirements(x: 1337)
    }
    """
    executableDocument.parse(doc4) match
      case Right(_ -> doc) => assert(clue(validate(doc, schemaDoc)).isLeft)
      case _               => fail("failed to parse doc4")
  }

  test("fragments") {
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
      case Right(_ -> doc) => assert(clue(validate(doc, schemaDoc)).isRight)
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
      case Right(_ -> doc) => assert(clue(validate(doc, schemaDoc)).isLeft)
      case _               => fail("failed to parse doc2")
  }

end ValidationSuite

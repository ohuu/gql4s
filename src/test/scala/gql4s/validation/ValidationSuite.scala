// Copyright (c) 2018-2021 by Oli Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s
package validation

import cats.data.NonEmptyList
import parsers.*
import munit.FunSuite

class ValidationSuite extends FunSuite:
  // val schema = new Fixture[TypeSystemDocument]("schema") {
  //   private var schema: TypeSystemDocument = null

  //   def apply() = schema

  //   override def beforeAll(): Unit = {
  //     schema = typeSystemDocument.parse(schemaStr) match
  //       case Right("" -> schema) => schema
  //       case _                   => fail("failed to parse schema")
  //   }
  // }

  // override def munitFixtures = List(schema)

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
      case Right(_ -> doc) => assert(operationNameUniqueness(doc).isRight)
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
      case Right(_ -> doc) => assert(operationNameUniqueness(doc).isLeft)
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
      case Right(_ -> doc) => assert(loneAnonOperation(doc).isRight)
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
      case Right(_ -> doc) => assert(loneAnonOperation(doc).isLeft)
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
      case Right(_ -> doc) => assert(subscriptionSingleRoot(doc).isRight)
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
      case Right(_ -> doc) => assert(subscriptionSingleRoot(doc).isRight)
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
      case Right(_ -> doc) => assert(subscriptionSingleRoot(doc).isLeft)
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
      case Right(_ -> doc) => assert(subscriptionSingleRoot(doc).isLeft)
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
      case Right(_ -> doc) => assert(fieldsExist(doc, schemaDoc).isRight)
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
      case Right(_ -> doc) => assert(fieldsExist(doc, schemaDoc).isRight)
      case _               => fail("failed to parse doc2")

  }

end ValidationSuite

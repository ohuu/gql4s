// Copyright (c) 2021 by Oliver Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s

import cats.data.NonEmptyList
import Type.*
import Value.*

val schemaStr = """
  type Query {
    dog: Dog
    multipleRequirements(x: Int!, y: Int!): Int!
  }

  enum DogCommand {
    SIT
    DOWN
    HEEL
  }

  type Dog implements Pet {
    name: String!
    nickname: String
    barkVolume: Int
    doesKnowCommand(dogCommand: DogCommand!): Boolean!
    isHouseTrained(atOtherHomes: Boolean): Boolean!
    owner: Human
  }

  interface Sentient {
    name: String!
    thoughts: String
  }

  interface Pet {
    name: String!
  }

  type Alien implements Sentient {
    name: String!
    homePlanet: String
  }

  type Human implements Sentient {
    name: String!
    pets: [Pet!]
  }

  enum CatCommand {
    JUMP
  }

  type Cat implements Pet {
    name: String!
    nickname: String
    doesKnowCommand(catCommand: CatCommand!): Boolean!
    meowVolume: Int
  }

  union CatOrDog = Cat | Dog
  union DogOrHuman = Dog | Human
  union HumanOrAlien = Human | Alien
"""

val schemaDoc = NonEmptyList.of(
  ObjectTypeDefinition(
    Name("Query"),
    Nil,
    Nil,
    List(
      FieldDefinition(Name("dog"), Nil, NamedType(Name("Dog")), Nil),
      FieldDefinition(
        Name("multipleRequirements"),
        List(
          InputValueDefinition(Name("x"), NonNullType(NamedType(Name("Int"))), None, Nil),
          InputValueDefinition(Name("y"), NonNullType(NamedType(Name("Int"))), None, Nil)
        ),
        NonNullType(NamedType(Name("Int"))),
        Nil
      )
    )
  ),
  EnumTypeDefinition(
    Name("DogCommand"),
    Nil,
    List(
      EnumValueDefinition(EnumValue(Name("SIT")), Nil),
      EnumValueDefinition(EnumValue(Name("DOWN")), Nil),
      EnumValueDefinition(EnumValue(Name("HEEL")), Nil)
    )
  ),
  ObjectTypeDefinition(
    Name("Dog"),
    List(NamedType(Name("Pet"))),
    Nil,
    List(
      FieldDefinition(Name("name"), Nil, NonNullType(NamedType(Name("String"))), Nil),
      FieldDefinition(Name("nickname"), Nil, NamedType(Name("String")), Nil),
      FieldDefinition(Name("barkVolume"), Nil, NamedType(Name("Int")), Nil),
      FieldDefinition(
        Name("doesKnowCommand"),
        List(
          InputValueDefinition(
            Name("dogCommand"),
            NonNullType(NamedType(Name("DogCommand"))),
            None,
            Nil
          )
        ),
        NonNullType(NamedType(Name("Boolean"))),
        Nil
      ),
      FieldDefinition(
        Name("isHouseTrained"),
        List(InputValueDefinition(Name("atOtherHomes"), NamedType(Name("Boolean")), None, Nil)),
        NonNullType(NamedType(Name("Boolean"))),
        Nil
      ),
      FieldDefinition(Name("owner"), Nil, NamedType(Name("Human")), Nil)
    )
  ),
  InterfaceTypeDefinition(
    Name("Sentient"),
    Nil,
    Nil,
    List(
      FieldDefinition(Name("name"), Nil, NonNullType(NamedType(Name("String"))), Nil),
      FieldDefinition(Name("thoughts"), Nil, NamedType(Name("String")), Nil)
    )
  ),
  InterfaceTypeDefinition(
    Name("Pet"),
    Nil,
    Nil,
    List(FieldDefinition(Name("name"), Nil, NonNullType(NamedType(Name("String"))), Nil))
  ),
  ObjectTypeDefinition(
    Name("Alien"),
    List(NamedType(Name("Sentient"))),
    Nil,
    List(
      FieldDefinition(Name("name"), Nil, NonNullType(NamedType(Name("String"))), Nil),
      FieldDefinition(Name("homePlanet"), Nil, NamedType(Name("String")), Nil)
    )
  ),
  ObjectTypeDefinition(
    Name("Human"),
    List(NamedType(Name("Sentient"))),
    Nil,
    List(
      FieldDefinition(Name("name"), Nil, NonNullType(NamedType(Name("String"))), Nil),
      FieldDefinition(Name("pets"), Nil, ListType(NonNullType(NamedType(Name("Pet")))), Nil)
    )
  ),
  EnumTypeDefinition(
    Name("CatCommand"),
    Nil,
    List(EnumValueDefinition(EnumValue(Name("JUMP")), Nil))
  ),
  ObjectTypeDefinition(
    Name("Cat"),
    List(NamedType(Name("Pet"))),
    Nil,
    List(
      FieldDefinition(Name("name"), Nil, NonNullType(NamedType(Name("String"))), Nil),
      FieldDefinition(Name("nickname"), Nil, NamedType(Name("String")), Nil),
      FieldDefinition(
        Name("doesKnowCommand"),
        List(
          InputValueDefinition(
            Name("catCommand"),
            NonNullType(NamedType(Name("CatCommand"))),
            None,
            Nil
          )
        ),
        NonNullType(NamedType(Name("Boolean"))),
        Nil
      ),
      FieldDefinition(Name("meowVolume"), Nil, NamedType(Name("Int")), Nil)
    )
  ),
  UnionTypeDefinition(
    Name("CatOrDog"),
    Nil,
    List(NamedType(Name("Cat")), NamedType(Name("Dog")))
  ),
  UnionTypeDefinition(
    Name("DogOrHuman"),
    Nil,
    List(NamedType(Name("Dog")), NamedType(Name("Human")))
  ),
  UnionTypeDefinition(
    Name("HumanOrAlien"),
    Nil,
    List(NamedType(Name("Human")), NamedType(Name("Alien")))
  )
)

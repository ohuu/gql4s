// Copyright (c) 2021 by Oliver Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s

import cats.data.NonEmptyList

import parsing.*
import parsing.Type.*
import parsing.Value.*

val schemaStr = """
  directive @myDir(x: Int!) on FIELD

  type Query {
    dog: Dog
    testInt(x: Int): Int
    testFloat(x: Float): Int
    testBoolean(x: Boolean): Int
    multipleRequirements(x: Int!, y: Int!): Int!
    findDog(complex: ComplexInput): Dog
    booleanList(booleanListArg: [Boolean!]): Boolean
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

  input Name {
    first: String
    last: String
  }
  
  input ComplexInput {
    name: Name!
    owner: String
  }
"""

val schemaDoc: TypeSystemDocument = TypeSystemDocument(
    definitions = List(
        DirectiveDefinition(
            Name("myDir"),
            List(
                InputValueDefinition(
                    Name("x"),
                    NonNullType(NamedType(Name("Int"))),
                    None,
                    Nil
                )
            ),
            false,
            List(ExecutableDirectiveLocation.FIELD)
        ),
        ObjectTypeDefinition(
            Name("Query"),
            Nil,
            Nil,
            List(
                FieldDefinition(Name("dog"), Nil, NamedType(Name("Dog")), Nil),
                FieldDefinition(
                    Name("testInt"),
                    List(InputValueDefinition(Name("x"), NamedType(Name("Int")), None, Nil)),
                    NamedType(Name("Int")),
                    Nil
                ),
                FieldDefinition(
                    Name("testFloat"),
                    List(InputValueDefinition(Name("x"), NamedType(Name("Float")), None, Nil)),
                    NamedType(Name("Int")),
                    Nil
                ),
                FieldDefinition(
                    Name("testBoolean"),
                    List(InputValueDefinition(Name("x"), NamedType(Name("Boolean")), None, Nil)),
                    NamedType(Name("Int")),
                    Nil
                ),
                FieldDefinition(
                    Name("multipleRequirements"),
                    List(
                        InputValueDefinition(Name("x"), NonNullType(NamedType(Name("Int"))), None, Nil),
                        InputValueDefinition(Name("y"), NonNullType(NamedType(Name("Int"))), None, Nil)
                    ),
                    NonNullType(NamedType(Name("Int"))),
                    Nil
                ),
                FieldDefinition(
                    Name("findDog"),
                    List(InputValueDefinition(Name("complex"), NamedType(Name("ComplexInput")), None, Nil)),
                    NamedType(Name("Dog")),
                    Nil
                ),
                FieldDefinition(
                    Name("booleanList"),
                    List(
                        InputValueDefinition(
                            Name("booleanListArg"),
                            ListType(NonNullType(NamedType(Name("Boolean")))),
                            None,
                            Nil
                        )
                    ),
                    NamedType(Name("Boolean")),
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
        ),
        InputObjectTypeDefinition(
            Name("Name"),
            Nil,
            List(
                InputValueDefinition(Name("first"), NamedType(Name("String")), None, Nil),
                InputValueDefinition(Name("last"), NamedType(Name("String")), None, Nil)
            )
        ),
        InputObjectTypeDefinition(
            Name("ComplexInput"),
            Nil,
            List(
                InputValueDefinition(Name("name"), NonNullType(NamedType(Name("Name"))), None, Nil),
                InputValueDefinition(Name("owner"), NamedType(Name("String")), None, Nil)
            )
        )
    )
)

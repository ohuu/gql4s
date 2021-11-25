// Copyright (c) 2018-2021 by Oli Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package adt

import cats.data.NonEmptyList

// Name
case class Name(name: String)

// Type
enum Type(val name: Name):
  case NamedType(n: Name) extends Type(n)
  case NonNullType(n: Name) extends Type(n)
  case ListType(tpe: Type) extends Type(tpe.name)

// Value
case class VariableDefinition(name: Name, tpe: Type, defaultValue: Option[Value])
case class ObjectField(name: Name, value: Value)
enum Value:
  case Variable(name: Name)
  case IntValue(value: Int)
  case FloatValue(value: Float)
  case StringValue(value: String)
  case BooleanValue(value: Boolean)
  case NullValue
  case ListValue(values: List[Value])
  case EnumValue(name: Name)
  case ObjectValue(fields: List[ObjectField])

// Argument
case class Argument(name: Name, value: Value)

// Directive
case class Directive(name: Name, arguments: List[Argument])

// Fragments
case class FragmentDefinition(
    name: Name,
    tpe: Type,
    directives: List[Directive],
    selectionSet: NonEmptyList[Selection]
) extends ExecutableDefinition

enum Selection:
  case Field(
      alias: Option[Name],
      name: Name,
      arguments: List[Argument],
      directives: List[Directive],
      selectionSet: List[Selection]
  )
  case InlineFragment(
      tpe: Option[Type],
      directives: List[Directive],
      selectionSet: NonEmptyList[Selection]
  )
  case FragmentSpread(name: Name, directivess: List[Directive])

// Operations
enum OperationType:
  case Query, Mutation, Subscription

case class OperationDefinition(
    operationType: OperationType,
    name: Option[Name],
    variableDefinitions: List[VariableDefinition],
    directives: List[Directive],
    selectionSet: NonEmptyList[Selection]
) extends ExecutableDefinition

// Documents
type Document           = NonEmptyList[Definition]
type ExecutableDocument = NonEmptyList[ExecutableDefinition]

trait Definition
trait ExecutableDefinition            extends Definition
trait TypeSystemDefinitionOrExtension extends Definition

///////////////////////////////////////////////////////////////////////////////////////////////////
// Type System
case class RootOperationTypeDefinition(operationType: OperationType, namedType: Type.NamedType)
case class SchemaDefinition(
    directives: List[Directive],
    rootOperationTypeDefinition: NonEmptyList[RootOperationTypeDefinition]
)

case class ScalarTypeDefinition(name: Name, directivess: List[Directive])
case class ScalarTypeExtension(name: Name, directives: NonEmptyList[Directive])

case class InputValueDefinition(
    name: Name,
    tpe: Type,
    defaultValue: Option[Value],
    directives: List[Directive]
)
case class FieldDefinition(
    name: Name,
    argumentsDefinition: List[InputValueDefinition],
    tpe: Type,
    directives: List[Directive]
)
case class ObjectTypeDefinition(
    name: Name,
    implementsInterfaces: List[Type.NamedType],
    directives: List[Directive],
    fieldsDefintion: List[FieldDefinition]
)
enum ObjectTypeExtension(val name: Name):
  case ObjectTypeExtension0(
      override val name: Name,
      implementsInterfaces: List[Type.NamedType],
      directives: List[Directive],
      fieldsDefinition: NonEmptyList[FieldDefinition]
  ) extends ObjectTypeExtension(name)
  case ObjectTypeExtension1(
      override val name: Name,
      implementsInterfaces: List[Type.NamedType],
      directives: NonEmptyList[Directive]
  ) extends ObjectTypeExtension(name)
  case ObjectTypeExtension2(
      override val name: Name,
      implementsInterfaces: NonEmptyList[Type.NamedType]
  ) extends ObjectTypeExtension(name)

case class InterfaceTypeDefinition(
    name: Name,
    implementsInterfaces: List[Type.NamedType],
    directives: List[Directive],
    fieldsDefinition: List[FieldDefinition]
)

enum InterfaceTypeExtension(val name: Name):
  case InterfaceTypeExtension0(
      override val name: Name,
      implementsInterfaces: List[Type.NamedType],
      directives: List[Directive],
      fieldsDefinition: NonEmptyList[FieldDefinition]
  ) extends InterfaceTypeExtension(name)
  case InterfaceTypeExtension1(
      override val name: Name,
      implementsInterfaces: List[Type.NamedType],
      directives: NonEmptyList[Directive]
  ) extends InterfaceTypeExtension(name)
  case InterfaceTypeExtension2(
      override val name: Name,
      implementsInterfaces: NonEmptyList[Type.NamedType]
  ) extends InterfaceTypeExtension(name)

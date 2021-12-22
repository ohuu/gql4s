// Copyright (c) 2018-2021 by Oli Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s

import cats.data.NonEmptyList

import Type.*
import Value.*

///////////////////////////////////////////////////////////////////////////////////////////////////
// Definitions & Documents
type Document           = NonEmptyList[Definition]
type ExecutableDocument = NonEmptyList[ExecutableDefinition]
type TypeSystemDocument = NonEmptyList[TypeSystemDefinition]

sealed trait Definition
sealed trait ExecutableDefinition extends Definition {
  def name: Option[Name]
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// Queries
case class Name(name: String)

// Types
enum Type(val name: Name):
  case NamedType(n: Name) extends Type(n)
  case NonNullType(tpe: Type) extends Type(tpe.name)
  case ListType(tpe: Type) extends Type(tpe.name)

// Values
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

// Arguments
case class Argument(name: Name, value: Value)

// Directives
case class Directive(name: Name, arguments: List[Argument])

// Fragments
case class FragmentDefinition(
    override val name: Some[Name],
    on: NamedType,
    directives: List[Directive],
    selectionSet: NonEmptyList[Selection]
) extends ExecutableDefinition

type SelectionSet = List[Selection]
enum Selection:
  case Field(
      alias: Option[Name],
      name: Name,
      arguments: List[Argument],
      directives: List[Directive],
      selectionSet: List[Selection]
  )
  case InlineFragment(
      onType: Option[Type.NamedType],
      directives: List[Directive],
      selectionSet: NonEmptyList[Selection]
  )
  case FragmentSpread(name: Name, directives: List[Directive])

// Operations
enum OperationType:
  case Query, Mutation, Subscription

case class OperationDefinition(
    operationType: OperationType,
    override val name: Option[Name],
    variableDefinitions: List[VariableDefinition],
    directives: List[Directive],
    selectionSet: NonEmptyList[Selection]
) extends ExecutableDefinition

///////////////////////////////////////////////////////////////////////////////////////////////////
// Schema
// Helper traits
sealed trait HasName:
  val name: Name

sealed trait ImplementsInterfaces:
  val interfaces: List[NamedType]

sealed trait TypeSystemDefinition            extends Definition
sealed trait TypeSystemExtension             extends Definition
sealed trait TypeSystemDefinitionOrExtension extends Definition

sealed trait TypeDefinition extends TypeSystemDefinition, HasName
sealed trait TypeExtension  extends TypeSystemExtension

case class RootOperationTypeDefinition(operationType: OperationType, namedType: NamedType)

case class SchemaDefinition(
    directives: List[Directive],
    rootOperationTypeDefinition: NonEmptyList[RootOperationTypeDefinition]
) extends TypeSystemDefinition

case class SchemaExtension(directives: List[Directive], root: List[RootOperationTypeDefinition])
    extends TypeSystemExtension

// Scalar Type
case class ScalarTypeDefinition(name: Name, directives: List[Directive]) extends TypeDefinition
case class ScalarTypeExtension(name: Name, directives: NonEmptyList[Directive])
    extends TypeExtension

// Object Type
case class InputValueDefinition(
    name: Name,
    tpe: Type,
    defaultValue: Option[Value],
    directives: List[Directive]
)
case class FieldDefinition(
    name: Name,
    arguments: List[InputValueDefinition],
    tpe: Type,
    directives: List[Directive]
)
case class ObjectTypeDefinition(
    name: Name,
    interfaces: List[NamedType],
    directives: List[Directive],
    fields: List[FieldDefinition]
) extends TypeDefinition

case class ObjectTypeExtension(
    name: Name,
    interfaces: List[NamedType],
    directives: List[Directive],
    fields: List[FieldDefinition]
) extends TypeExtension

// Interfaces
case class InterfaceTypeDefinition(
    name: Name,
    interfaces: List[NamedType],
    directives: List[Directive],
    fields: List[FieldDefinition]
) extends TypeDefinition

case class InterfaceTypeExtension(
    name: Name,
    interfaces: List[NamedType],
    directives: List[Directive],
    fields: List[FieldDefinition]
) extends TypeExtension

// Unions
case class UnionTypeDefinition(
    name: Name,
    directives: List[Directive],
    unionMemberTypes: List[Type.NamedType]
) extends TypeDefinition

case class UnionTypeExtension(
    name: Name,
    directives: List[Directive],
    unionMembers: List[NamedType]
) extends TypeExtension

// Enum Type Definition
case class EnumValueDefinition(value: EnumValue, directives: List[Directive])

case class EnumTypeDefinition(
    name: Name,
    directives: List[Directive],
    values: List[EnumValueDefinition]
) extends TypeDefinition

case class EnumTypeExtension(
    name: Name,
    directives: List[Directive],
    values: List[EnumValueDefinition]
) extends TypeExtension

// Input Objects
case class InputObjectTypeDefinition(
    name: Name,
    directives: List[Directive],
    fieldsDef: List[InputValueDefinition]
) extends TypeDefinition

case class InputObjectTypeExtension(
    name: Name,
    directives: List[Directive],
    fieldsDef: List[InputValueDefinition]
) extends TypeExtension

// Directives
sealed trait DirectiveLocation

enum TypeSystemDirectiveLocation extends DirectiveLocation:
  case SCHEMA, SCALAR, OBJECT, FIELD_DEFINITION, ARGUMENT_DEFINITION, INTERFACE, UNION, ENUM,
  ENUM_VALUE, INPUT_OBJECT, INPUT_FIELD_DEFINITION

enum ExecutableDirectiveLocation extends DirectiveLocation:
  case QUERY, MUTATION, SUBSCRIPTION, FIELD, FRAGMENT_DEFINITION, FRAGMENT_SPREAD, INLINE_FRAGMENT,
  VARIABLE_DEFINITION

case class DirectiveDefinition(
    name: Name,
    arguments: List[InputValueDefinition],
    repeatable: Boolean,
    directiveLocs: NonEmptyList[DirectiveLocation]
) extends TypeSystemDefinition

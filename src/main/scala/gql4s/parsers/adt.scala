// Copyright (c) 2018-2021 by Oli Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s
package parsers

import cats.data.NonEmptyList
import cats.parse.{Parser as P, Parser0 as P0}
import cats.parse.Parser.*
import cats.parse.Rfc5234.{alpha, cr, crlf, hexdig, htab, lf, wsp}
import cats.parse.Numbers.{digit, nonZeroDigit}

// import OperationType.*
// import Selection.*
import Type.*
import Value.*

///////////////////////////////////////////////////////////////////////////////////////////////////
// Definitions & Documents
type Document           = NonEmptyList[Definition]
type ExecutableDocument = NonEmptyList[ExecutableDefinition]

trait Definition
trait ExecutableDefinition extends Definition {
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
    override val name: Option[Name],
    variableDefinitions: List[VariableDefinition],
    directives: List[Directive],
    selectionSet: NonEmptyList[Selection]
) extends ExecutableDefinition

// ///////////////////////////////////////////////////////////////////////////////////////////////////
// // Schema
trait TypeSystemDefinition            extends Definition
trait TypeSystemExtension             extends Definition
trait TypeSystemDefinitionOrExtension extends Definition

trait TypeDefinition extends TypeSystemDefinition
trait TypeExtension  extends TypeSystemExtension

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
trait DirectiveLocation

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

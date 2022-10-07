// Copyright (c) 2021 by Oliver Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s
package parsing

import scala.annotation.tailrec
import scala.reflect.TypeTest

import OperationType.*
import Type.*
import Value.*

sealed trait HasType:
    def `type`: Type

sealed trait HasName:
    def name: Name

sealed trait HasFields:
    def fields: List[FieldDefinition]

sealed trait HasArgs:
    def arguments: List[InputValueDefinition]

sealed trait HasInterfaces:
    def interfaces: List[NamedType]

sealed trait HasSelectionSet:
    def selectionSet: List[Selection]

sealed trait HasDirectives:
    def directives: List[Directive]

///////////////////////////////////////////////////////////////////////////////////////////////////
// Definitions & Documents
case class ExecutableDocument(definitions: List[ExecutableDefinition])

case class TypeSystemDocument(definitions: List[TypeSystemDefinition]):
    val builtInScalarDefs = List(
        ScalarTypeDefinition(Name("Int"), Nil),
        ScalarTypeDefinition(Name("Float"), Nil),
        ScalarTypeDefinition(Name("String"), Nil),
        ScalarTypeDefinition(Name("Boolean"), Nil),
        ScalarTypeDefinition(Name("ID"), Nil)
    )

    val builtInDirectiveDefs = List(
        DirectiveDefinition(
            Name("skip"),
            List(InputValueDefinition(Name("if"), NonNullType(NamedType(Name("Boolean"))), None, Nil)),
            false,
            List(
                ExecutableDirectiveLocation.FIELD,
                ExecutableDirectiveLocation.FRAGMENT_SPREAD,
                ExecutableDirectiveLocation.INLINE_FRAGMENT
            )
        ),
        DirectiveDefinition(
            Name("include"),
            List(InputValueDefinition(Name("if"), NonNullType(NamedType(Name("Boolean"))), None, Nil)),
            false,
            List(
                ExecutableDirectiveLocation.FIELD,
                ExecutableDirectiveLocation.FRAGMENT_SPREAD,
                ExecutableDirectiveLocation.INLINE_FRAGMENT
            )
        ),
        DirectiveDefinition(
            Name("deprecated"),
            List(InputValueDefinition(Name("reason"), NamedType(Name("String")), None, Nil)),
            false,
            List(
                TypeSystemDirectiveLocation.FIELD_DEFINITION,
                TypeSystemDirectiveLocation.ENUM_VALUE
            )
        ),
        DirectiveDefinition(
            Name("specifiedBy"),
            List(InputValueDefinition(Name("url"), NonNullType(NamedType(Name("String"))), None, Nil)),
            false,
            List(TypeSystemDirectiveLocation.SCALAR)
        )
    )
end TypeSystemDocument

sealed trait Definition
sealed trait ExecutableDefinition extends Definition, HasName, HasDirectives, HasSelectionSet

///////////////////////////////////////////////////////////////////////////////////////////////////
// Queries
case class Name(text: String)

// Types
enum Type(val name: Name) extends HasName:
    case NamedType(override val name: Name) extends Type(name)
    case NonNullType(`type`: Type)          extends Type(`type`.name)
    case ListType(`type`: Type)             extends Type(`type`.name)

// Values
case class VariableDefinition(
    name: Name,
    `type`: Type,
    defaultValue: Option[Value],
    directives: List[Directive]
) extends HasType,
      HasName,
      HasDirectives

case class ObjectField(name: Name, value: Value) extends HasName
enum Value:
    case Variable(name: Name)
    case IntValue(value: String)
    case FloatValue(value: String)
    case StringValue(value: String)
    case BooleanValue(value: String)
    case NullValue
    case ListValue(values: List[Value])
    case EnumValue(name: Name)
    case ObjectValue(fields: List[ObjectField])

// Arguments
case class Argument(name: Name, value: Value) extends HasName

// Directives
case class Directive(name: Name, arguments: List[Argument]) extends HasName

// Fragments
case class FragmentDefinition(
    override val name: Name,
    on: NamedType,
    directives: List[Directive],
    selectionSet: List[Selection]
) extends ExecutableDefinition

sealed trait Selection // TODO: this should be an enum! Once HasName is a typeclass see if you can add an instance of HasName[FragmentSpread]

case class Field(
    alias: Option[Name],
    name: Name,
    arguments: List[Argument],
    directives: List[Directive],
    selectionSet: List[Selection]
) extends Selection,
      HasName,
      HasDirectives,
      HasSelectionSet

case class InlineFragment(
    onType: Option[Type.NamedType],
    directives: List[Directive],
    selectionSet: List[Selection]
) extends Selection,
      HasDirectives,
      HasSelectionSet

case class FragmentSpread(name: Name, directives: List[Directive]) extends Selection, HasName, HasDirectives

// Operations
enum OperationType:
    case Query, Mutation, Subscription

case class OperationDefinition(
    name: Name,
    operationType: OperationType,
    variableDefinitions: List[VariableDefinition],
    directives: List[Directive],
    selectionSet: List[Selection]
) extends ExecutableDefinition

///////////////////////////////////////////////////////////////////////////////////////////////////
// Schema
sealed trait TypeSystemDefinition            extends Definition
sealed trait TypeSystemExtension             extends Definition
sealed trait TypeSystemDefinitionOrExtension extends Definition

sealed trait TypeDefinition extends TypeSystemDefinition, HasName, HasDirectives:
    def name: Name
    def directives: List[Directive]

sealed trait TypeExtension extends TypeSystemExtension, HasName:
    def name: Name

case class RootOperationTypeDefinition(operationType: OperationType, namedType: NamedType)

case class SchemaDefinition(
    directives: List[Directive],
    rootOperationTypeDefinition: List[RootOperationTypeDefinition]
) extends TypeSystemDefinition,
      HasDirectives

case class SchemaExtension(directives: List[Directive], root: List[RootOperationTypeDefinition])
    extends TypeSystemExtension,
      HasDirectives

// Scalar Type
case class ScalarTypeDefinition(name: Name, directives: List[Directive]) extends TypeDefinition, HasDirectives
case class ScalarTypeExtension(name: Name, directives: List[Directive])  extends TypeExtension, HasDirectives

// Object Type
case class InputValueDefinition(
    name: Name,
    `type`: Type,
    defaultValue: Option[Value],
    directives: List[Directive]
) extends HasType,
      HasName,
      HasDirectives

case class FieldDefinition(
    name: Name,
    arguments: List[InputValueDefinition],
    `type`: Type,
    directives: List[Directive]
) extends HasType,
      HasName,
      HasArgs,
      HasDirectives

sealed trait ObjectLikeTypeDefinition extends TypeDefinition, HasName, HasFields, HasInterfaces, HasDirectives:
    def name: Name
    def fields: List[FieldDefinition]
    def interfaces: List[NamedType]
    def directives: List[Directive]

case class ObjectTypeDefinition(
    name: Name,
    interfaces: List[NamedType],
    directives: List[Directive],
    fields: List[FieldDefinition]
) extends ObjectLikeTypeDefinition

case class ObjectTypeExtension(
    name: Name,
    interfaces: List[NamedType],
    directives: List[Directive],
    fields: List[FieldDefinition]
) extends TypeExtension,
      HasDirectives

// Interfaces
case class InterfaceTypeDefinition(
    name: Name,
    interfaces: List[NamedType],
    directives: List[Directive],
    fields: List[FieldDefinition]
) extends ObjectLikeTypeDefinition

case class InterfaceTypeExtension(
    name: Name,
    interfaces: List[NamedType],
    directives: List[Directive],
    fields: List[FieldDefinition]
) extends TypeExtension,
      HasDirectives

// Unions
case class UnionTypeDefinition(
    name: Name,
    directives: List[Directive],
    unionMemberTypes: List[Type.NamedType]
) extends TypeDefinition,
      HasDirectives

case class UnionTypeExtension(
    name: Name,
    directives: List[Directive],
    unionMembers: List[NamedType]
) extends TypeExtension,
      HasDirectives

// Enum Type Definition
case class EnumValueDefinition(value: EnumValue, directives: List[Directive]) extends HasName, HasDirectives:
    export value.*

case class EnumTypeDefinition(
    name: Name,
    directives: List[Directive],
    values: List[EnumValueDefinition]
) extends TypeDefinition,
      HasDirectives

case class EnumTypeExtension(
    name: Name,
    directives: List[Directive],
    values: List[EnumValueDefinition]
) extends TypeExtension,
      HasDirectives

// Input Objects
case class InputObjectTypeDefinition(
    name: Name,
    directives: List[Directive],
    fields: List[InputValueDefinition]
) extends TypeDefinition,
      HasArgs,
      HasDirectives:
    def arguments = fields // Although these appear as fields they are also (kind of) arguments

case class InputObjectTypeExtension(
    name: Name,
    directives: List[Directive],
    fieldsDef: List[InputValueDefinition]
) extends TypeExtension,
      HasDirectives

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
    directiveLocs: List[DirectiveLocation]
) extends TypeSystemDefinition,
      HasName,
      HasArgs

// Copyright (c) 2018-2021 by Oli Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package parsers

import cats.data.NonEmptyList
import cats.parse.{Parser as P, Parser0 as P0}
import cats.parse.Parser.{char, charIn, eitherOr, recursive, string}

import EnumTypeDefinition.*
import EnumTypeExtension.*
import InputObjectTypeDefinition.*
import InputObjectTypeExtension.*
import InterfaceTypeDefinition.*
import InterfaceTypeExtension.*
import ObjectTypeDefinition.*
import ObjectTypeExtension.*
import SchemaExtension.*
import UnionTypeExtension.*
import Type.*
import Value.*

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST
trait TypeSystemDefinition
trait TypeSystemExtension

trait TypeDefinition extends TypeSystemDefinition
trait TypeExtension  extends TypeSystemExtension

case class RootOperationTypeDefinition(operationType: OperationType, namedType: NamedType)
case class SchemaDefinition(
    directives: List[Directive],
    rootOperationTypeDefinition: NonEmptyList[RootOperationTypeDefinition]
) extends TypeSystemDefinition
enum SchemaExtension extends TypeSystemExtension:
  case SchemaExtensionA(
      directives: List[Directive],
      root: NonEmptyList[RootOperationTypeDefinition]
  )
  case SchemaExtensionB(directives: NonEmptyList[Directive])

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
enum ObjectTypeDefinition extends TypeDefinition:
  case ObjectTypeDefinitionA(
      name: Name,
      interfaces: List[NamedType],
      directives: List[Directive],
      fields: NonEmptyList[FieldDefinition]
  )
  case ObjectTypeDefinitionB(
      name: Name,
      interfaces: List[NamedType],
      directives: List[Directive]
  )

enum ObjectTypeExtension extends TypeExtension:
  case ObjectTypeExtensionA(
      name: Name,
      interfaces: List[NamedType],
      directives: List[Directive],
      fields: NonEmptyList[FieldDefinition]
  )
  case ObjectTypeExtensionB(
      name: Name,
      interfaces: List[NamedType],
      directives: NonEmptyList[Directive]
  )
  case ObjectTypeExtensionC(
      name: Name,
      interfaces: NonEmptyList[NamedType]
  )

// Interfaces
enum InterfaceTypeDefinition extends TypeDefinition:
  case InterfaceTypeDefinitionA(
      name: Name,
      interfaces: List[NamedType],
      directives: List[Directive],
      fields: NonEmptyList[FieldDefinition]
  )
  case InterfaceTypeDefinitionB(
      name: Name,
      interfaces: List[NamedType],
      directives: List[Directive]
  )

enum InterfaceTypeExtension extends TypeExtension:
  case InterfaceTypeExtensionA(
      name: Name,
      interfaces: List[NamedType],
      directives: List[Directive],
      fields: NonEmptyList[FieldDefinition]
  )
  case InterfaceTypeExtensionB(
      name: Name,
      interfaces: List[NamedType],
      directives: NonEmptyList[Directive]
  )
  case InterfaceTypeExtensionC(
      name: Name,
      interfaces: NonEmptyList[NamedType]
  )

// Unions
case class UnionTypeDefinition(
    name: Name,
    directives: List[Directive],
    unionMemberTypes: List[Type.NamedType]
) extends TypeDefinition

enum UnionTypeExtension extends TypeExtension:
  case UnionTypeExtensionA(
      name: Name,
      directives: List[Directive],
      unionMembers: NonEmptyList[NamedType]
  )
  case UnionTypeExtensionB(
      name: Name,
      directives: NonEmptyList[Directive]
  )

// Enum Type Definition
case class EnumValueDefinition(value: EnumValue, directives: List[Directive])

enum EnumTypeDefinition extends TypeDefinition:
  case EnumTypeDefinitionA(
      name: Name,
      directives: List[Directive],
      values: NonEmptyList[EnumValueDefinition]
  )
  case EnumTypeDefinitionB(name: Name, directives: List[Directive])

enum EnumTypeExtension extends TypeExtension:
  case EnumTypeExtensionA(
      name: Name,
      directives: List[Directive],
      values: NonEmptyList[EnumValueDefinition]
  )
  case EnumTypeExtensionB(name: Name, directives: NonEmptyList[Directive])

// Input Objects
enum InputObjectTypeDefinition extends TypeDefinition:
  case InputObjectTypeDefinitionA(
      name: Name,
      directives: List[Directive],
      fieldsDef: NonEmptyList[InputValueDefinition]
  )
  case InputObjectTypeDefinitionB(name: Name, directives: List[Directive])

enum InputObjectTypeExtension extends TypeExtension:
  case InputObjectTypeExtensionA(
      name: Name,
      directives: List[Directive],
      fieldsDef: NonEmptyList[InputValueDefinition]
  )
  case InputObjectTypeExtensionB(name: Name, directives: NonEmptyList[Directive])

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

/////////////////////////////////////////////////////////////////////////////////////////////////
// Parsers
// Helpers
val extend = (string("extend") ~ __).void

// Description
val desc = (stringValue.? ~ __).void.with1

// Type System
val typeSystemDefinition = schemaDefinition | typeDefinition | directiveDefinition
val typeSystemExtension  = schemaExtension | typeExtension

val typeSystemDefinitionOrExtension = eitherOr(typeSystemDefinition, typeSystemExtension)
val typeSystemExtensionDocument     = typeSystemDefinitionOrExtension.rep

val typeSystemDocument = typeSystemDefinition.rep

// Type
val typeDefinition =
  scalarTypeDefinition |
    objectTypeDefinition |
    interfaceTypeDefinition |
    unionTypeDefinition |
    enumTypeDefinition |
    inputObjectTypeDefinition

val typeExtension =
  scalarTypeExtension |
    objectTypeExtension |
    interfaceTypeExtension |
    unionTypeExtension |
    enumTypeExtension |
    inputObjectTypeExtension

// Schema
val schema = string("schema")
val rootOperationTypeDefinition = ((operationType <* __ ~ char(':') ~ __) ~ namedType).map {
  case operationType -> namedType => RootOperationTypeDefinition(operationType, namedType)
}
val rootOperationTypeDefinitions =
  (char('{') ~ __).with1 *> (rootOperationTypeDefinition <* __).rep <* char('}')
val schemaDefinition =
  ((desc ~ schema ~ __) *> (directives0 <* __).with1 ~ rootOperationTypeDefinitions)
    .map { case directives -> rootOperationTypeDefinitions =>
      SchemaDefinition(directives, rootOperationTypeDefinitions)
    }

val schemaExtensionA = ((directives0 <* __) ~ rootOperationTypeDefinitions).map {
  case dirs -> root => SchemaExtensionA(dirs, root)
}
val schemaExtensionB = ((directives <* __) <* !char('{')).map { case dirs =>
  SchemaExtensionB(dirs)
}
val schemaExtension = extend ~ schema *> schemaExtensionA.backtrack | schemaExtensionB

// Scalar
val scalar = (string("scalar") ~ __).void
val scalarTypeDefinition = (desc ~ scalar *> (name <* __) ~ directives0).map {
  case name -> directives => ScalarTypeDefinition(name, directives)
}
val scalarTypeExtension = ((desc ~ extend ~ scalar) *> (name <* __) ~ directives)
  .map { case name -> directives => ScalarTypeExtension(name, directives) }

// Object
val inputValueDefinition =
  (desc *> (name <* __ ~ char(':') ~ __) ~ (`type` <* __) ~ (defaultValue.? <* __) ~ directives0)
    .map { case name -> tpe -> defaultValue -> directives =>
      InputValueDefinition(name, tpe, defaultValue, directives)
    }
val argumentsDefinition  = char('(') ~ __ *> (inputValueDefinition <* __).rep <* char(')')
val argumentsDefinition0 = argumentsDefinition.?.map(_.map(_.toList).getOrElse(Nil))
val fieldDefinition =
  (desc *>
    (name <* __) ~ (argumentsDefinition0 <* __ ~ char(':') ~ __) ~ (`type` <* __) ~ directives0)
    .map { case name -> argumentsDefinition -> tpe -> directives =>
      FieldDefinition(name, argumentsDefinition, tpe, directives)
    }
val fieldsDefinition  = char('{') ~ __ *> (fieldDefinition <* __).rep <* char('}')
val fieldsDefinition0 = fieldsDefinition.?.map(_.map(_.toList).getOrElse(Nil))
val implementsInterfaces = recursive[NonEmptyList[NamedType]] { recurse =>
  (((string("implements") | char('&')).? ~ __).with1 *> (namedType <* __) ~ recurse.?).map {
    case tpe -> None       => NonEmptyList.one(tpe)
    case tpe -> Some(tpes) => tpe :: tpes
  }
}
val implementsInterfaces0 = implementsInterfaces.?.map(_.map(_.toList).getOrElse(Nil))

val typeStr = (string("type") ~ __).void
val objectTypeDefinitionA =
  ((name <* __) ~ (implementsInterfaces0 <* __) ~ (directives0 <* __) ~ fieldsDefinition).map {
    case name -> impls -> dirs -> fields =>
      ObjectTypeDefinitionA(name, impls, dirs, fields)
  }
val objectTypeDefinitionB =
  ((name <* __) ~ (implementsInterfaces0 <* __) ~ directives0).map { case name -> impls -> dirs =>
    ObjectTypeDefinitionB(name, impls, dirs)
  }
val objectTypeDefinition = desc ~ typeStr *> objectTypeDefinitionA.backtrack | objectTypeDefinitionB

val extendType = (extend ~ string("type") ~ __).void
val objectTypeExtensionA =
  ((name <* __) ~ (implementsInterfaces0 <* __) ~ (directives0 <* __) ~ fieldsDefinition).map {
    case name -> impls -> dirs -> fields => ObjectTypeExtensionA(name, impls, dirs, fields)
  }
val objectTypeExtensionB =
  ((name <* __) ~ (implementsInterfaces0 <* __) ~ directives <* !char('{')).map {
    case name -> impls -> dirs => ObjectTypeExtensionB(name, impls, dirs)
  }
val objectTypeExtensionC = ((name <* __) ~ (implementsInterfaces <* __) <* !char('{')).map {
  case name -> impls => ObjectTypeExtensionC(name, impls)
}
val objectTypeExtension =
  extendType *> (objectTypeExtensionA.backtrack | objectTypeExtensionB.backtrack | objectTypeExtensionC)

// Interfaces
val interface = (desc ~ string("interface") ~ __).void
val interfaceTypeDefinitionA =
  ((name <* __) ~ (implementsInterfaces0 <* __) ~ (directives0 <* __) ~ fieldsDefinition)
    .map { case name -> impls -> dirs -> fields =>
      InterfaceTypeDefinitionA(name, impls, dirs, fields)
    }
val interfaceTypeDefinitionB = ((name <* __) ~ (implementsInterfaces0 <* __) ~ directives0)
  .map { case name -> impls -> dirs => InterfaceTypeDefinitionB(name, impls, dirs) }
val interfaceTypeDefinition =
  interface *> interfaceTypeDefinitionA.backtrack | interfaceTypeDefinitionB

val extendInterface = (extend ~ string("interface") ~ __).void
val interfaceTypeExtensionA =
  ((name <* __) ~ (implementsInterfaces0 <* __) ~ (directives0 <* __) ~ fieldsDefinition).map {
    case name -> impls -> dirs -> fields =>
      InterfaceTypeExtensionA(name, impls, dirs, fields)
  }
val interfaceTypeExtensionB =
  ((name <* __) ~ (implementsInterfaces0 <* __) ~ directives <* !char('{')).map {
    case name -> impls -> dirs => InterfaceTypeExtensionB(name, impls, dirs)
  }
val interfaceTypeExtensionC = ((name <* __) ~ (implementsInterfaces <* __) <* !char('{')).map {
  case name -> impls => InterfaceTypeExtensionC(name, impls)
}
val interfaceTypeExtension =
  extendInterface *> (interfaceTypeExtensionA.backtrack | interfaceTypeExtensionB.backtrack | interfaceTypeExtensionC)

// Union
val unionMemberType = recursive[NonEmptyList[NamedType]] { recurse =>
  (((char('=') | char('|')).? ~ __).with1 *> (namedType <* __) ~ recurse.?).map {
    case tpe -> None       => NonEmptyList.one(tpe)
    case tpe -> Some(tpes) => tpe :: tpes
  }
}
val unionMemberType0 = unionMemberType.?.map(_.map(_.toList).getOrElse(Nil))
val unionTypeDefinition =
  (desc ~ string("union") ~ __ *> (name <* __) ~ (directives0 <* __) ~ unionMemberType0).map {
    case name -> dirs -> unionMemberTypes => UnionTypeDefinition(name, dirs, unionMemberTypes)
  }

val extendUnion = (extend ~ string("union") ~ __).void
val unionTypeExtensionA = ((name <* __) ~ (directives0 <* __) ~ unionMemberType).map {
  case name -> dirs -> unionMembers => UnionTypeExtensionA(name, dirs, unionMembers)
}
val unionTypeExtensionB = ((name <* __) ~ directives).map { case name -> dirs =>
  UnionTypeExtensionB(name, dirs)
}
val unionTypeExtension = extendUnion *> (unionTypeExtensionA.backtrack | unionTypeExtensionB)

// Enums
val enumValueDefinition = (desc *> (enumValue <* __) ~ directives0).map { case enumVal -> dirs =>
  EnumValueDefinition(enumVal, dirs)
}
val enumValuesDefinition = char('{') ~ __ *> enumValueDefinition.rep <* char('}')

val enumStr = (string("enum") ~ __).void
val enumTypeDefinitionA =
  ((name <* __) ~ (directives0 <* __) ~ enumValuesDefinition).map { case name -> dirs -> values =>
    EnumTypeDefinitionA(name, dirs, values)
  }
val enumTypeDefinitionB =
  ((name <* __) ~ (directives0 <* __) <* !char('{')).map { case name -> dirs =>
    EnumTypeDefinitionB(name, dirs)
  }
val enumTypeDefinition = desc ~ enumStr *> enumTypeDefinitionA.backtrack | enumTypeDefinitionB

val extendEnum = (extend ~ string("enum") ~ __).void
val enumTypeExtensionA = ((name <* __) ~ (directives0 <* __) ~ enumValuesDefinition).map {
  case name -> dirs -> values => EnumTypeExtensionA(name, dirs, values)
}
val enumTypeExtensionB = ((name <* __) ~ (directives <* __) <* !char('{')).map {
  case name -> dirs => EnumTypeExtensionB(name, dirs)
}
val enumTypeExtension = extendEnum *> enumTypeExtensionA.backtrack | enumTypeExtensionB

// Input Objects
val inputFieldsDefinition  = char('{') ~ __ *> inputValueDefinition.rep <* char('}')
val inputFieldsDefinition0 = inputFieldsDefinition.?.map(_.map(_.toList).getOrElse(Nil))

val input = (desc ~ string("input") ~ __).void
val inputObjectTypeDefinitionA = ((name <* __) ~ (directives0 <* __) ~ inputFieldsDefinition).map {
  case name -> dirs -> fields => InputObjectTypeDefinitionA(name, dirs, fields)
}
val inputObjectTypeDefinitionB = ((name <* __) ~ (directives0 <* __) <* !char('{')).map {
  case name -> dirs => InputObjectTypeDefinitionB(name, dirs)
}
val inputObjectTypeDefinition = input *> inputObjectTypeDefinitionA | inputObjectTypeDefinitionB

val extendInput = (desc ~ extend ~ string("input") ~ __).void
val inputObjectTypeExtensionA = ((name <* __) ~ (directives0 <* __) ~ inputFieldsDefinition).map {
  case name -> dirs -> fields => InputObjectTypeExtensionA(name, dirs, fields)
}
val inputObjectTypeExtensionB = ((name <* __) ~ (directives <* __) <* !char('{')).map {
  case name -> dirs => InputObjectTypeExtensionB(name, dirs)
}
val inputObjectTypeExtension =
  extendInput *> inputObjectTypeExtensionA.backtrack | inputObjectTypeExtensionB

// Directives
import TypeSystemDirectiveLocation.*
import ExecutableDirectiveLocation.*

val typeSystemDirectiveLocation =
  string("SCHEMA").map(_ => SCHEMA) |
    string("SCALAR").map(_ => SCALAR) |
    string("OBJECT").map(_ => OBJECT) |
    string("FIELD_DEFINITION").map(_ => FIELD_DEFINITION) |
    string("ARGUMENT_DEFINITION").map(_ => ARGUMENT_DEFINITION) |
    string("INTERFACE").map(_ => INTERFACE) |
    string("UNION").map(_ => UNION) |
    string("ENUM").map(_ => ENUM) |
    string("ENUM_VALUE").map(_ => ENUM_VALUE) |
    string("INPUT_OBJECT").map(_ => INPUT_OBJECT) |
    string("INPUT_FIELD_DEFINITION").map(_ => INPUT_FIELD_DEFINITION)

val executableDirectiveLocation =
  string("QUERY").map(_ => QUERY) |
    string("MUTATION").map(_ => MUTATION) |
    string("SUBSCRIPTION").map(_ => SUBSCRIPTION) |
    string("FIELD").map(_ => FIELD) |
    string("FRAGMENT_DEFINITION").map(_ => FRAGMENT_DEFINITION) |
    string("FRAGMENT_SPREAD").map(_ => FRAGMENT_SPREAD) |
    string("INLINE_FRAGMENT").map(_ => INLINE_FRAGMENT) |
    string("VARIABLE_DEFINITION").map(_ => VARIABLE_DEFINITION)

val directiveLocation = typeSystemDirectiveLocation | executableDirectiveLocation

val directiveLocations = recursive[NonEmptyList[DirectiveLocation]] { recurse =>
  (((char('|')).? ~ __).with1 *> (directiveLocation <* __) ~ recurse.?).map {
    case tpe -> None       => NonEmptyList.one(tpe)
    case tpe -> Some(tpes) => tpe :: tpes
  }
}

val directiveStart = (desc ~ string("directive") ~ __ ~ char('@') ~ __).void
val on             = (__ ~ string("on") ~ __).void
val repeatable     = (string("repeatable").? <* on).map(_.isDefined)
val directiveDefinition =
  (directiveStart *> (name <* __) ~ (argumentsDefinition0 <* __) ~ repeatable ~ directiveLocations)
    .map { case name -> args -> repeatable -> dirs =>
      DirectiveDefinition(name, args, repeatable, dirs)
    }

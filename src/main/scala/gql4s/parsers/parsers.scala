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

import TypeSystemDirectiveLocation.*
import ExecutableDirectiveLocation.*
import OperationType.*
import Selection.*
import Type.*
import Value.*

///////////////////////////////////////////////////////////////////////////////////////////////////
// Queries
// Source Character
// val except = charIn('\u0000' to '\u0008')
//   | charIn('\u000B' to '\u000C')
//   | charIn('\u000E' to '\u001F')
// val sourceCharacter = until(except)
val sourceCharacter = (charIn('\u0020' to '\uffff') | cr | lf | htab).string

// Unicode BOM
val unicodeBom = char('\uFEFF')

// Line Terminator
val lineTerminator = crlf | cr | lf

// Comment
val commentChar = (sourceCharacter ~ !lineTerminator).void
val comment     = (char('#') ~ commentChar.rep0).void

// Ignore
val __ = (unicodeBom | wsp | lineTerminator | comment | char(',')).rep0.void

// // Name
val namePart = ((char('_') | alpha) ~ (char('_') | alpha | digit).rep0).string
val name     = namePart.map(Name(_))

// Int Value
val integerPart = (char('-').?.with1 ~ (char('0') | nonZeroDigit ~ digit.rep0)).string
val intValue    = integerPart.string.map(s => IntValue(s.toInt))

// Float Value
val sign              = charIn('-', '+')
val exponentIndicator = charIn('e', 'E')
val exponentPart      = (exponentIndicator ~ sign.? ~ digit.rep).string
val fractionalPart    = (char('.') ~ digit.rep).string
val floatValue =
  (integerPart ~ (exponentPart | (fractionalPart ~ exponentPart.?)) ~
    !(char('.') | alpha)).string
    .map(_.toFloat)
    .map(FloatValue(_))

// Boolean Value
val booleanValue = (string("true") | string("false")).string
  .map(_.toBoolean)
  .map(BooleanValue(_))

// // String Value
val quote            = char('"')
val triQuote         = string("\"\"\"")
val escTriQuote      = string("\\\"\"\"")
val escapedCharacter = charIn('"', '\\', '/', 'b', 'f', 'n', 'r', 't')
val escapedUnicode   = hexdig.rep(4, 4).string
val blockStringCharacter =
  (!(triQuote | escTriQuote)).with1 *> sourceCharacter | escTriQuote.string
val blockString = triQuote *> blockStringCharacter.rep0.string <* triQuote
val stringCharacter =
  ((string("\\u") ~ escapedUnicode).string |
    (char('\\') ~ escapedCharacter).string |
    (!(char('"') | char('\\') | lineTerminator)).with1 *> sourceCharacter)
val stringValue =
  (blockString |
    (!char('"')).with1 *> string("\"\"").map(_ => "") |
    quote *> stringCharacter.rep.string <* quote)
    .map(StringValue(_))

// Null Value
val nullValue = string("null").map(_ => NullValue)

// Enum Value
val enumValue: P[EnumValue] =
  ((!(booleanValue | nullValue)).with1 *> name).map(EnumValue(_))

// List Value
val listValue: P[ListValue] = (char('[') ~ __ *> (defer(value) <* __).rep0 <* char(']'))
  .map(ListValue(_))

// Object Value
val objectField: P[ObjectField] = ((name <* __ ~ char(':') ~ __) ~ defer(value)).map {
  case name -> value => ObjectField(name, value)
}
val objectValue =
  (char('{') ~ __ *> (objectField <* __).rep0 <* char('}')).map(ObjectValue(_))

// // Type References
val namedType: P[NamedType] = name.map(NamedType(_))
val listType: P[Type]       = (char('[') ~ __ *> defer(`type`) <* char(']')).map(ListType(_))
val `type` = ((namedType | listType) ~ char('!').?).map {
  case (tpe, None) => tpe
  case (tpe, _)    => NonNullType(tpe)
}

// // Variable
val defaultValue          = (char('=') ~ __ *> defer(value))
val variable: P[Variable] = char('$') ~ __ *> name.map(Variable(_))
val variableDefinition =
  ((variable <* __ ~ char(':') ~ __) ~ (`type` <* __) ~ defaultValue.?).map {
    case Variable(name) -> tpe -> defaultValue => VariableDefinition(name, tpe, defaultValue)
  }
val variableDefinitions  = (char('(') ~ __ *> (variableDefinition <* __).rep <* char(')'))
val variableDefinitions0 = variableDefinitions.?.map(_.map(_.toList).getOrElse(Nil))

val value =
  variable
    | floatValue.backtrack
    | intValue
    | stringValue
    | booleanValue
    | nullValue
    | enumValue
    | listValue
    | objectValue

// Arguments
val argument = ((name <* __ ~ char(':') ~ __) ~ value).map { case name -> value =>
  Argument(name, value)
}
val arguments  = (char('(') ~ __ *> (argument <* __).rep <* char(')'))
val arguments0 = arguments.?.map(_.map(_.toList).getOrElse(Nil))

// Directives
val directive = (char('@') ~ __ *> (name <* __) ~ arguments0).map { case name -> arguments =>
  Directive(name, arguments)
}
val directives  = (directive <* __).rep
val directives0 = (directive <* __).rep0

// Selection Set
val selection: P[Selection] =
  defer(inlineFragment).backtrack | defer(fragmentSpread) | defer(field)
val selectionSet  = (char('{') ~ __ *> (selection <* __).rep <* char('}'))
val selectionSet0 = selectionSet.?.map(_.map(_.toList).getOrElse(Nil))

// Fragments
val fragment      = string("fragment")
val typeCondition = string("on") ~ __ *> namedType
val fragmentName  = (!string("on")).with1 *> name
val fragmentDefinition =
  ((fragment ~ __) *> (fragmentName <* __) ~ (typeCondition <* __) ~ (directives0 <* __) ~ selectionSet)
    .map { case name -> typeCondition -> directives -> selectionSet =>
      FragmentDefinition(Some(name), typeCondition, directives, selectionSet)
    }
val fragmentSpread = (string("...") ~ __ *> (fragmentName <* __) ~ directives0).map {
  case name -> directives => FragmentSpread(name, directives)
}
val inlineFragment =
  (string("...") ~ __ *> (typeCondition.? <* __) ~ (directives0 <* __) ~ selectionSet).map {
    case tpe -> directives -> selectionSet => InlineFragment(tpe, directives, selectionSet)
  }

// Fields
val withAlias = (char(':') *> __) *> name
val field =
  ((name <* __) ~ (withAlias.? <* __) ~ (arguments0 <* __) ~ (directives0 <* __) ~ selectionSet0)
    .map {
      case name -> None -> arguments -> directives -> selectionSet =>
        Field(None, name, arguments, directives, selectionSet)
      case alias -> Some(name) -> arguments -> directives -> selectionSet =>
        Field(Some(alias), name, arguments, directives, selectionSet)
    }

// Operations
val query         = string("query").map(_ => Query)
val mutation      = string("mutation").map(_ => Mutation)
val subscription  = string("subscription").map(_ => Subscription)
val operationType = query | mutation | subscription
val operationDefinition =
  (((operationType <* __) ~ (name.? <* __) ~ (variableDefinitions0 <* __) ~ (directives0 <* __)).?.with1 ~ selectionSet)
    .map {
      case None -> selectionSet => OperationDefinition(Query, None, Nil, Nil, selectionSet)
      case Some(operationType -> name -> variableDefinitions -> directives) -> selectionSet =>
        OperationDefinition(operationType, name, variableDefinitions, directives, selectionSet)
    }

/////////////////////////////////////////////////////////////////////////////////////////////////
// Schema
val extend = (string("extend") ~ __).void

// Description
val desc = (stringValue.? ~ __).void.with1

// Type System
val typeSystemDefinition = defer(schemaDefinition | typeDefinition | directiveDefinition)
val typeSystemExtension  = defer(schemaExtension | typeExtension)

val typeSystemDefinitionOrExtension = typeSystemDefinition | typeSystemExtension
val typeSystemExtensionDocument     = typeSystemDefinitionOrExtension.rep

val typeSystemDocument = typeSystemDefinition.rep

// // Type
val typeDefinition = defer(
  scalarTypeDefinition |
    objectTypeDefinition |
    interfaceTypeDefinition |
    unionTypeDefinition |
    enumTypeDefinition |
    inputObjectTypeDefinition
)

val typeExtension = defer(
  scalarTypeExtension |
    objectTypeExtension |
    interfaceTypeExtension |
    unionTypeExtension |
    enumTypeExtension |
    inputObjectTypeExtension
)

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
  case dirs -> root => SchemaExtension(dirs, root.toList)
}
val schemaExtensionB = ((directives <* __) <* !char('{')).map { case dirs =>
  SchemaExtension(dirs.toList, Nil)
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
val fieldsDefinition = char('{') ~ __ *> (fieldDefinition <* __).rep <* char('}')
// val fieldsDefinition0 = fieldsDefinition.?.map(_.map(_.toList).getOrElse(Nil))
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
      ObjectTypeDefinition(name, impls, dirs, fields.toList)
  }
val objectTypeDefinitionB =
  ((name <* __) ~ (implementsInterfaces0 <* __) ~ directives0).map { case name -> impls -> dirs =>
    ObjectTypeDefinition(name, impls, dirs, Nil)
  }
val objectTypeDefinition =
  desc ~ typeStr *> objectTypeDefinitionA.backtrack | objectTypeDefinitionB

val extendType = (extend ~ string("type") ~ __).void
val objectTypeExtensionA =
  ((name <* __) ~ (implementsInterfaces0 <* __) ~ (directives0 <* __) ~ fieldsDefinition).map {
    case name -> impls -> dirs -> fields => ObjectTypeExtension(name, impls, dirs, fields.toList)
  }
val objectTypeExtensionB =
  ((name <* __) ~ (implementsInterfaces0 <* __) ~ directives <* !char('{')).map {
    case name -> impls -> dirs => ObjectTypeExtension(name, impls, dirs.toList, Nil)
  }
val objectTypeExtensionC = ((name <* __) ~ (implementsInterfaces <* __) <* !char('{')).map {
  case name -> impls => ObjectTypeExtension(name, impls.toList, Nil, Nil)
}
val objectTypeExtension =
  extendType *> (objectTypeExtensionA.backtrack | objectTypeExtensionB.backtrack | objectTypeExtensionC)

// Interfaces
val interface = (desc ~ string("interface") ~ __).void
val interfaceTypeDefinitionA =
  ((name <* __) ~ (implementsInterfaces0 <* __) ~ (directives0 <* __) ~ fieldsDefinition)
    .map { case name -> impls -> dirs -> fields =>
      InterfaceTypeDefinition(name, impls, dirs, fields.toList)
    }
val interfaceTypeDefinitionB = ((name <* __) ~ (implementsInterfaces0 <* __) ~ directives0)
  .map { case name -> impls -> dirs => InterfaceTypeDefinition(name, impls, dirs, Nil) }
val interfaceTypeDefinition =
  interface *> interfaceTypeDefinitionA.backtrack | interfaceTypeDefinitionB

val extendInterface = (extend ~ string("interface") ~ __).void
val interfaceTypeExtensionA =
  ((name <* __) ~ (implementsInterfaces0 <* __) ~ (directives0 <* __) ~ fieldsDefinition).map {
    case name -> impls -> dirs -> fields =>
      InterfaceTypeExtension(name, impls, dirs, fields.toList)
  }
val interfaceTypeExtensionB =
  ((name <* __) ~ (implementsInterfaces0 <* __) ~ directives <* !char('{')).map {
    case name -> impls -> dirs => InterfaceTypeExtension(name, impls, dirs.toList, Nil)
  }
val interfaceTypeExtensionC = ((name <* __) ~ (implementsInterfaces <* __) <* !char('{')).map {
  case name -> impls => InterfaceTypeExtension(name, impls.toList, Nil, Nil)
}
val interfaceTypeExtension =
  extendInterface *> (interfaceTypeExtensionA.backtrack | interfaceTypeExtensionB.backtrack | interfaceTypeExtensionC)

// Union
val unionMemberType = recursive[NonEmptyList[NamedType]] { recurse =>
  ((char('|').? ~ __).with1 *> (namedType <* __) ~ recurse.?).map {
    case tpe -> None       => NonEmptyList.one(tpe)
    case tpe -> Some(tpes) => tpe :: tpes
  }
}
val unionMemberType0 = unionMemberType.?.map(_.map(_.toList).getOrElse(Nil))
val unionTypeDefinition =
  (desc ~ string("union") ~ __ *> (name <* __) ~ (directives0 <* __ ~ char(
    '='
  ) <* __) ~ unionMemberType0).map { case name -> dirs -> unionMemberTypes =>
    UnionTypeDefinition(name, dirs, unionMemberTypes)
  }

val extendUnion = (extend ~ string("union") ~ __).void
val unionTypeExtensionA =
  ((name <* __) ~ (directives0 <* __ ~ char('=') ~ __) ~ unionMemberType).map {
    case name -> dirs -> unionMembers => UnionTypeExtension(name, dirs, unionMembers.toList)
  }
val unionTypeExtensionB = ((name <* __) ~ (directives <* __ ~ char('=') ~ __)).map {
  case name -> dirs => UnionTypeExtension(name, dirs.toList, Nil)
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
    EnumTypeDefinition(name, dirs, values.toList)
  }
val enumTypeDefinitionB =
  ((name <* __) ~ (directives0 <* __) <* !char('{')).map { case name -> dirs =>
    EnumTypeDefinition(name, dirs, Nil)
  }
val enumTypeDefinition = desc ~ enumStr *> enumTypeDefinitionA.backtrack | enumTypeDefinitionB

val extendEnum = (extend ~ string("enum") ~ __).void
val enumTypeExtensionA = ((name <* __) ~ (directives0 <* __) ~ enumValuesDefinition).map {
  case name -> dirs -> values => EnumTypeExtension(name, dirs, values.toList)
}
val enumTypeExtensionB = ((name <* __) ~ (directives <* __) <* !char('{')).map {
  case name -> dirs => EnumTypeExtension(name, dirs.toList, Nil)
}
val enumTypeExtension = extendEnum *> enumTypeExtensionA.backtrack | enumTypeExtensionB

// Input Objects
val inputFieldsDefinition = char('{') ~ __ *> inputValueDefinition.rep <* char('}')
// val inputFieldsDefinition0 = inputFieldsDefinition.?.map(_.map(_.toList).getOrElse(Nil))

val input = (desc ~ string("input") ~ __).void
val inputObjectTypeDefinitionA =
  ((name <* __) ~ (directives0 <* __) ~ inputFieldsDefinition).map { case name -> dirs -> fields =>
    InputObjectTypeDefinition(name, dirs, fields.toList)
  }
val inputObjectTypeDefinitionB = ((name <* __) ~ (directives0 <* __) <* !char('{')).map {
  case name -> dirs => InputObjectTypeDefinition(name, dirs, Nil)
}
val inputObjectTypeDefinition = input *> inputObjectTypeDefinitionA | inputObjectTypeDefinitionB

val extendInput = (desc ~ extend ~ string("input") ~ __).void
val inputObjectTypeExtensionA = ((name <* __) ~ (directives0 <* __) ~ inputFieldsDefinition).map {
  case name -> dirs -> fields => InputObjectTypeExtension(name, dirs, fields.toList)
}
val inputObjectTypeExtensionB = ((name <* __) ~ (directives <* __) <* !char('{')).map {
  case name -> dirs => InputObjectTypeExtension(name, dirs.toList, Nil)
}
val inputObjectTypeExtension =
  extendInput *> inputObjectTypeExtensionA.backtrack | inputObjectTypeExtensionB

// Directives
val typeSystemDirectiveLocation =
  string("SCHEMA").map(_ => SCHEMA) |
    string("SCALAR").map(_ => SCALAR) |
    string("OBJECT").map(_ => OBJECT) |
    string("FIELD_DEFINITION").map(_ => FIELD_DEFINITION) |
    string("ARGUMENT_DEFINITION").map(_ => ARGUMENT_DEFINITION) |
    string("INTERFACE").map(_ => INTERFACE) |
    string("UNION").map(_ => UNION) |
    string("ENUM_VALUE").map(_ => ENUM_VALUE) |
    string("ENUM").map(_ => ENUM) |
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

// ///////////////////////////////////////////////////////////////////////////////////////////////////
// // Definitions & Documents
val executableDefinition = operationDefinition | fragmentDefinition
val executableDocument   = __ *> (executableDefinition <* __).rep

val definition = executableDefinition | typeSystemDefinitionOrExtension
val document   = __ *> (definition <* __).rep

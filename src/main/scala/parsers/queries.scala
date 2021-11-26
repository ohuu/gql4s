// Copyright (c) 2018-2021 by Oli Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package parsers

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST
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

///////////////////////////////////////////////////////////////////////////////////////////////////
// Parsers
import cats.parse.{Parser as P, Parser0 as P0}
import cats.parse.Parser.{char, charIn, defer, string}
import cats.parse.Rfc5234.{alpha, cr, crlf, hexdig, htab, lf, wsp}
import cats.parse.Numbers.{digit, nonZeroDigit}

import OperationType.*
import Selection.*
import Type.*
import Value.*

// Documents
type Document           = NonEmptyList[Definition]
type ExecutableDocument = NonEmptyList[ExecutableDefinition]

trait Definition
trait ExecutableDefinition            extends Definition
trait TypeSystemDefinitionOrExtension extends Definition

// Source Character
val except = charIn('\u0000' to '\u0008')
  | charIn('\u000B' to '\u000C')
  | charIn('\u000E' to '\u001F')
// val sourceCharacter = until(except)
val sourceCharacter = (charIn('\u0020' to '\uffff') | cr | lf | htab).string

// Unicode BOM
val unicodeBom = char('\uFEFF')

// Line Terminator
val lineTerminator = crlf | cr | lf

// Comment
val commentChar = (sourceCharacter ~ !lineTerminator).void
val comment     = (char('#') ~ commentChar.rep0).void

// Insignificant Comma
val comma = char(',')

// Ignore
val __ = (unicodeBom | wsp | lineTerminator | comment | comma).rep0.void

// Name
val namePart  = ((char('_') | alpha) ~ (char('_') | alpha | digit).rep0).string
val nameStart = alpha
val name      = namePart.map(Name(_))

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
    !(char('.') | nameStart)).string
    .map(_.toFloat)
    .map(FloatValue(_))

// Boolean Value
val booleanValue = (string("true") | string("false")).string
  .map(_.toBoolean)
  .map(BooleanValue(_))

// String Value
val triQuote             = string("\"\"\"")
val escTriQuote          = string("\\\"\"\"")
val blockStringCharacter = (!triQuote).with1 *> (escTriQuote | sourceCharacter).string
val blockStringValue     = triQuote *> blockStringCharacter.rep0.string <* triQuote
val escapedCharacter     = charIn('"', '\\', '/', 'b', 'f', 'n', 'r', 't')
val escapedUnicode       = hexdig.rep(4, 4).string
val stringValue0         = (string("\\u") ~ escapedUnicode).string // unicode
val stringValue1         = (char('\\') ~ escapedCharacter).string  // escape code
val stringValue2         = (!(char('"') | char('\\') | lineTerminator)).with1 *> sourceCharacter
val stringValue =
  (char('"') *> (stringValue0 | stringValue1 | stringValue2).rep.string <* char('"'))
    .map(StringValue(_))

// Null Value
val nullValue = string("null").map(_ => NullValue)

// Enum Value
val enumValue: P[EnumValue] = ((!(booleanValue | nullValue)).with1 *> name).map(EnumValue(_))

// List Value
val listValue: P[ListValue] = (char('[') ~ __ *> (defer(value) <* __).rep0 <* char(']'))
  .map(ListValue(_))

// Object Value
val objectField: P[ObjectField] = ((name <* __ ~ char(':') ~ __) ~ defer(value)).map {
  case name -> value => ObjectField(name, value)
}
val objectValue = (char('{') ~ __ *> (objectField <* __).rep0 <* char('}')).map(ObjectValue(_))

// Type References
val namedType: P[NamedType] = name.map(NamedType(_))
val listType: P[Type]       = (char('[') ~ __ *> defer(`type`) <* char(']')).map(ListType(_))
val `type` = ((namedType | listType) ~ (__ *> char('!')).?).map {
  case (tpe, None) => tpe
  case (tpe, _)    => NonNullType(tpe.name)
}

// Variable
val defaultValue          = (char('=') ~ __ *> defer(value))
val variable: P[Variable] = char('$') ~ __ *> name.map(Variable(_))
val variableDefinition = ((variable <* __ ~ char(':') ~ __) ~ (`type` <* __) ~ defaultValue.?).map {
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
val selection: P[Selection] = defer(inlineFragment).backtrack | defer(fragmentSpread) | defer(field)
val selectionSet            = (char('{') ~ __ *> (selection <* __).rep <* char('}'))
val selectionSet0           = selectionSet.?.map(_.map(_.toList).getOrElse(Nil))

// Fragments
val fragment      = string("fragment")
val typeCondition = string("on") ~ __ *> namedType
val fragmentName  = (!string("on")).with1 *> name
val fragmentDefinition =
  ((fragment ~ __) *> (fragmentName <* __) ~ (typeCondition <* __) ~ (directives0 <* __) ~ selectionSet)
    .map { case name -> typeCondition -> directives -> selectionSet =>
      FragmentDefinition(name, typeCondition, directives, selectionSet)
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

// Document
val executableDefinition = operationDefinition | fragmentDefinition
val executableDocument   = __ *> (executableDefinition <* __).rep

val definition: P[Definition] = executableDefinition /* | typeSystemDefinitionOrExtension */
val document                  = __ *> (definition <* __).rep

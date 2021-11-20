// Copyright (c) 2018-2021 by Oli Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package parser

import adt.*
import adt.OperationDefinition.*
import adt.Selection.*
import adt.Type.*
import adt.Value.*
import cats.parse.{Parser as P, Parser0 as P0}
import cats.parse.Parser.{char, charIn, defer, defer0, string, until}
import cats.parse.Rfc5234.{alpha, cr, crlf, hexdig, htab, lf, wsp}
import cats.parse.Numbers.{digit, nonZeroDigit, signedIntString}

// Surrounds
// def betweenParas[T](p: P0[T]): P[T]   = char('(') *> ignore *> p <* ignore <* char(')')
// def betweenCurlys[T](p: P0[T]): P[T]  = char('{') *> ignore *> p <* ignore <* char('}')
// def betweenSquares[T](p: P0[T]): P[T] = char('[') *> ignore *> p <* ignore <* char(']')
// def betweenWsp[T](p: P[T]): P[T]      = wsp.rep0.with1 *> p <* wsp.rep0
// def betweenWspLn[T](p: P[T]): P[T]    = ignore.with1 *> p <* ignore

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
val ignore = (unicodeBom | wsp | lineTerminator | comment | comma).rep0

// Name
val namePart = ((char('_') | alpha) ~ (char('_') | alpha | digit).rep0).string
val name     = namePart.map(Name(_))

// Int Value
val negativeSign = char('-')
val integerPart  = ((negativeSign.?).with1 ~ (char('0') | nonZeroDigit ~ digit.rep0)).string
val intValue     = integerPart.string.map(s => IntValue(s.toInt))

// Float Value
val sign              = charIn('-', '+')
val exponentIndicator = charIn('e', 'E')
val exponentPart      = (exponentIndicator ~ sign.? ~ digit.rep).string
val fractionalPart    = (char('.') ~ digit.rep).string
val floatValue =
  (integerPart ~ (exponentPart | fractionalPart ~ exponentPart.?)).string
    .map(s => FloatValue(s.toFloat))

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
val nullValue = string("null")
  .map(_ => NullValue)

// Enum Value
val enumValue = ((!(booleanValue | nullValue)).with1 *> name)
  .map(EnumValue(_))

// List Value
val listValue: P[Value] =
  (char('[') ~ ignore *> (defer(value) <* ignore).rep0 <* char(']'))
    .map(_.toList)
    .map(ListValue(_))

// Object Value
val objectField: P[ObjectField] = ((name <* (ignore ~ char(':') ~ ignore)) ~ defer(value))
  .map(ObjectField.apply.tupled)
val objectValue =
  (char('{') ~ ignore *> (objectField <* ignore).rep0 <* char('}')).map(ObjectValue(_))

// Type References
val namedType         = name.map(NamedType(_))
val listType: P[Type] = (char('[') ~ ignore *> defer(tpe) <* ignore ~ char(']')).map(ListType(_))
val tpe = ((namedType | listType) ~ (ignore *> char('!')).?).map {
  case (parsedType, None) => parsedType
  case (parsedType, _)    => NonNullType(parsedType.name)
}

// Variable
val defaultValue          = char('=') ~ ignore *> defer(value)
val variable: P[Variable] = (char('$') ~ ignore *> name).map(Variable(_))
val variableDefinition =
  (variable ~ (ignore ~ char(':') ~ ignore *> tpe <* ignore) ~ defaultValue.?).map {
    case Variable(name) -> tpe -> defaultValue => VariableDefinition(name, tpe, defaultValue)
  }
val variableDefinitions = char('(') ~ ignore *> (variableDefinition <* ignore).rep <* char(')')

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
val argument  = ((name <* (ignore ~ char(':') ~ ignore)) ~ value).map(Argument.apply.tupled)
val arguments = char('(') ~ ignore *> (argument <* ignore).rep0 <* char(')')

// Directives
val directive  = (char('@') ~ ignore *> name ~ (ignore *> arguments.?)).map(Directive.apply.tupled)
val directives = (directive <* ignore).rep

// Selection Set
val selection: P[Selection] = defer(inlineFragment).backtrack | defer(fragmentSpread) | defer(field)
val selectionSet            = char('{') ~ ignore *> (selection <* ignore).rep <* char('}')

// Fragments
val typeCondition = string("on") ~ ignore *> namedType
val fragmentName  = (!string("on")).with1 *> name
val fragmentDefinition =
  (
    string("fragment") ~ ignore *> (fragmentName <* ignore) ~ (typeCondition <* ignore) ~
      directives.? ~ (selectionSet <* ignore)
  ).map {
    case name -> tpe -> None -> sels => FragmentDefinition(name, tpe, Nil, sels.toList)
    case name -> tpe -> Some(dirs) -> sels =>
      FragmentDefinition(name, tpe, dirs.toList, sels.toList)
  }
val fragmentSpread = (string("...") ~ ignore *> (fragmentName <* ignore) ~ directives.?).map {
  case (name, None)       => Selection.FragmentSpread(name, Nil)
  case (name, Some(dirs)) => Selection.FragmentSpread(name, dirs.toList)
}

val inlineFragment =
  (string("...") ~ ignore *> (typeCondition.? <* ignore) ~ directives.? ~ (selectionSet <* ignore))
    .map {
      case ((tpe, None), sels)       => InlineFragment(tpe, Nil, sels.toList)
      case ((tpe, Some(dirs)), sels) => InlineFragment(tpe, dirs.toList, sels.toList)
    }

// Fields
val withAlias = (char(':') *> ignore) *> name
val field =
  (
    (name <* ignore) ~ (withAlias.? <* ignore) ~ (arguments.? <* ignore) ~
      directives.? ~ (selectionSet.? <* ignore)
  )
    .map {
      case name -> None -> args -> dirs -> sel =>
        Field(
          None,
          name,
          args.getOrElse(Nil),
          dirs.map(_.toList).getOrElse(Nil),
          sel.map(_.toList).getOrElse(Nil)
        )

      case alias -> Some(name) -> args -> dirs -> sel =>
        Field(
          Some(alias),
          name,
          args.getOrElse(Nil),
          dirs.map(_.toList).getOrElse(Nil),
          sel.map(_.toList).getOrElse(Nil)
        )
    }

// Operations
val operationType =
  ((string("query") | string("mutation") | string("subscription")) <* ignore).string
val operationDefinition: P[OperationDefinition] =
  ((operationType ~ (name.? <* ignore) ~ (variableDefinitions.? <* ignore) ~ directives.?).?.with1 ~ (selectionSet <* ignore))
    .map {
      case None -> sel => Query(None, Nil, Nil, Nil)
      case Some("query" -> name -> vars -> dirs) -> sel =>
        Query(
          name,
          vars.map(_.toList).getOrElse(Nil),
          dirs.map(_.toList).getOrElse(Nil),
          sel.toList
        )
      case Some("mutation" -> name -> vars -> dirs) -> sel =>
        Mutation(
          name,
          vars.map(_.toList).getOrElse(Nil),
          dirs.map(_.toList).getOrElse(Nil),
          sel.toList
        )
      case Some("subscription" -> name -> vars -> dirs) -> sel =>
        Subscription(
          name,
          vars.map(_.toList).getOrElse(Nil),
          dirs.map(_.toList).getOrElse(Nil),
          sel.toList
        )
    }

// Document
val executableDefinition = operationDefinition | fragmentDefinition
val executableDocument   = ignore *> (executableDefinition <* ignore).rep

val definition: P[Definition] = executableDefinition /* | typeSystemDefinitionOrExtention */
val document                  = ignore *> (definition <* ignore).rep

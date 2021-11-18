// Copyright (c) 2018-2021 by Oli Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package parser

import adt.*
import adt.Value.*
import cats.parse.{Parser as P, Parser0 as P0}
import cats.parse.Parser.{char, charIn, string, defer, defer0, until}
import cats.parse.Rfc5234.{alpha, cr, crlf, hexdig, htab, lf, wsp}
import cats.parse.Numbers.{digit, nonZeroDigit, signedIntString}

// Source Character
val except = charIn('\u0000' to '\u0008')
  | charIn('\u000B' to '\u000C')
  | charIn('\u000E' to '\u001F')
// val sourceCharacter = until(except)
val sourceCharacter = (charIn('\u0020' to '\uffff') | cr | lf | htab).string

// Surrounds
def betweenParas[T](p: P0[T]): P[T]   = char('(') *> ignore *> p <* ignore <* char(')')
def betweenCurlys[T](p: P0[T]): P[T]  = char('{') *> ignore *> p <* ignore <* char('}')
def betweenSquares[T](p: P0[T]): P[T] = char('[') *> ignore *> p <* ignore <* char(']')
def betweenWsp[T](p: P[T]): P[T]      = wsp.rep0.with1 *> p <* wsp.rep0

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
val name = namePart
  .map(Name(_))

// Int Value
val negativeSign = char('-')
val integerPart  = ((negativeSign.?).with1 ~ (char('0') | nonZeroDigit ~ digit.rep)).string
val intValue = integerPart.string
  .map(s => IntValue(s.toInt))

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
  .map(s => BooleanValue(s.toBoolean))

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
  betweenSquares((defer(value) <* ignore).rep0)
    .map(_.toList)
    .map(ListValue(_))

// Object Value
val objectField: P[ObjectField] = ((name <* betweenWsp(char(':'))) ~ defer(value))
  .map(ObjectField.apply.tupled)
val objectValue = betweenCurlys((objectField <* ignore).rep0)
  .map(ObjectValue(_))

// Type References
val namedType           = name
val listType: P[String] = (char('[') ~ defer(tpe) ~ char(']')).string
val nonNullType         = ((namedType | listType) ~ char('!')).string
val tpe                 = ((namedType | listType) ~ char('!').?).string

// Variable
val defaultValue        = (char('=') ~ betweenWsp(defer(value))).string
val variable            = (char('$') *> name).map(Variable(_))
val variableDefinition  = (variable ~ char(':') ~ tpe ~ defaultValue).string
val variableDefinitions = betweenParas(variableDefinition.rep.string)

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
val argument  = ((name <* betweenWsp(char(':'))) ~ value).map(Argument.apply.tupled)
val arguments = betweenParas((argument <* ignore).rep0)

// Directives
val directive  = (char('@') *> name ~ arguments.?).map(Directive.apply.tupled)
val directives = directive.rep0

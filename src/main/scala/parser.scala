package parser

import adt.*
import adt.Value.*
import cats.parse.Parser as P
import cats.parse.Parser.{char, charIn, string, defer, defer0}
import cats.parse.Rfc5234.{alpha, cr, crlf, hexdig, htab, lf, wsp}
import cats.parse.Numbers.{digit, nonZeroDigit, signedIntString}

// Source Character
val sourceCharacter = (charIn((0x0020 to 0xffff).map(_.toChar)) | cr | lf | htab).string

// Line Terminator
val lineTerminator = crlf | cr | lf

// Insignificant Comma
val comma = char(',')

// Name
val namePart = ((char('_') | alpha) ~ (char('_') | alpha | digit).rep0).string
val name = namePart
  .surroundedBy(wsp.rep0)
  .map(Name(_))

// Int Value
val negativeSign = char('-')
val integerPart  = ((negativeSign.?).with1 ~ (char('0') | nonZeroDigit ~ digit.rep)).string
val intValue = integerPart.string
  .surroundedBy(wsp.rep0)
  .map(s => IntValue(s.toInt))

// Float Value
val sign              = charIn('-', '+')
val exponentIndicator = charIn('e', 'E')
val exponentPart      = (exponentIndicator ~ sign.? ~ digit.rep).string
val fractionalPart    = (char('.') ~ digit.rep).string
val floatValue =
  (integerPart ~ (exponentPart | fractionalPart ~ exponentPart.?)).string
    .surroundedBy(wsp.rep0)
    .map(s => FloatValue(s.toFloat))

// Boolean Value
val booleanValue = (string("true") | string("false")).string
  .surroundedBy(wsp.rep0)
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
    .surroundedBy(wsp.rep0)
    .map(StringValue(_))

// Null Value
val nullValue = string("null").surroundedBy(wsp.rep0).map(_ => NullValue)

// Enum Value
val enumValue = ((!(booleanValue | nullValue)).with1 *> name)
  .surroundedBy(wsp.rep0)
  .map(EnumValue(_))

// List Value
val listValue: P[Value] =
  (char('[') *> defer0((value <* comma.?).surroundedBy(wsp.rep0).rep0) <* char(']'))
    .surroundedBy(wsp.rep0)
    .map(ListValue(_))

// Object Value
val objectField: P[ObjectField] = (name ~ (char(':')).surroundedBy(wsp.rep0) ~ defer(value))
  .surroundedBy(wsp.rep0)
  .map { case ((name, _), value) =>
    ObjectField(name, value)
  }
val objectValue = (char('{') *> (objectField <* comma.?).rep0 <* char('}')).map(ObjectValue(_))

// Type References
val namedType           = name
val listType: P[String] = (char('[') ~ defer(_type) ~ char(']')).string
val nonNullType         = ((namedType | listType) ~ char('!')).string
val _type               = ((namedType | listType) ~ char('!').?).string

// Variable
val defaultValue        = (char('=') ~ defer(value)).string
val variable            = (char('$') *> name).map(Variable(_))
val variableDefinition  = (variable ~ char(':') ~ _type ~ defaultValue).string
val variableDefinitions = char('(') *> variableDefinition.rep.string <* char(')')

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

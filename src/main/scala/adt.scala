// Copyright (c) 2018-2021 by Oli Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package adt

// Name
case class Name(name: String)

// Value
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
end Value

// Argument
case class Argument(name: Name, value: Value)

// Directive
object Directive {
  def apply(name: Name, args: Option[List[Argument]]) = new Directive(name, args.getOrElse(Nil))
}
case class Directive(name: Name, args: List[Argument])

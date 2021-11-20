// Copyright (c) 2018-2021 by Oli Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package adt

import java.rmi.server.Operation

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
end Value

// Argument
case class Argument(name: Name, value: Value)

// Directive
object Directive {
  def apply(name: Name, args: Option[List[Argument]]) = new Directive(name, args.getOrElse(Nil))
}
case class Directive(name: Name, args: List[Argument])

// Fragments
case class FragmentDefinition(
    name: Name,
    tpe: Type,
    directives: List[Directive],
    selectionSet: List[Selection]
) extends ExecutableDefinition
enum Selection:
  case Field(
      alias: Option[Name],
      name: Name,
      args: List[Argument],
      dirs: List[Directive],
      sels: List[Selection]
  )
  case InlineFragment(
      tpe: Option[Type],
      directives: List[Directive],
      selectionSet: List[Selection]
  )
  case FragmentSpread(name: Name, directives: List[Directive])

// Operations
enum OperationDefinition(
    val name: Option[Name],
    val variables: List[VariableDefinition],
    val directives: List[Directive],
    val selectionSet: List[Selection]
) extends ExecutableDefinition:
  case Query(
      override val name: Option[Name],
      override val variables: List[VariableDefinition],
      override val directives: List[Directive],
      override val selectionSet: List[Selection]
  ) extends OperationDefinition(name, variables, directives, selectionSet)
  case Mutation(
      override val name: Option[Name],
      override val variables: List[VariableDefinition],
      override val directives: List[Directive],
      override val selectionSet: List[Selection]
  ) extends OperationDefinition(name, variables, directives, selectionSet)
  case Subscription(
      override val name: Option[Name],
      override val variables: List[VariableDefinition],
      override val directives: List[Directive],
      override val selectionSet: List[Selection]
  ) extends OperationDefinition(name, variables, directives, selectionSet)

// Documents
type Document           = List[Definition]
type ExecutableDocument = List[ExecutableDefinition]

trait Definition
trait ExecutableDefinition            extends Definition
trait TypeSystemDefinitionOrExtension extends Definition

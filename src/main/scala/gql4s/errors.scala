// Copyright (c) 2021 by Oliver Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s
package errors

import parsing.*
import parsing.Type.*

enum GqlError:
  case MissingOperationTypeDefinition(opType: OperationType)
  case MissingTypeDefinition(namedType: NamedType)
  case MissingFragmentDefinition(fragName: Name)
  case MissingField(fieldName: Name, parentType: NamedType)
  case MissingFields(tpe: NamedType)
  case MissingSelection(fieldName: Name, parentType: NamedType)
  case MissingArgumentDefinition(argName: Name, fieldName: Name, parentType: NamedType)
  case MissingArgument(argName: Name, fieldName: Name, parentType: NamedType)
  case MissingInputObjectTypeDefinition(typeName: Name)
  case MissingVariable(varName: Name)
  case MissingUnionMember(tpe: UnionTypeDefinition)
  case MissingEnumValue(tpe: EnumTypeDefinition)
  case IllegalSelection(fieldName: Name, namedType: NamedType)
  case IllegalType(tpe: Type, message: Option[String] = None)
  case IllegalName(name: Name)
  // case InvalidType(tpe: Type)
  // TODO: Should I rename this to InvalidReturnType or something
  case InvalidImplementation(tpe: Type)
  case DuplicateArgument(argName: Name, fieldName: Name, namedType: NamedType)
  case DuplicateFragmentDefinition(fragName: Name)
  case DuplicateOperationDefinition(opName: Name)
  case DuplicateField(fieldName: Name)
  case DuplicateVariable(varName: Name)
  case DuplicateInterface(interface: Name)
  case DuplicateValue(value: Name)
  case FragmentContainsCycles(fragName: Name)
  case InputObjectContainsCycles(inputObjName: Name)
  case MultipleAnonymousQueries
  case AnonymousQueryNotAlone
  case SubscriptionHasMultipleRoots(subName: Option[Name])
  case UnusedFragment(fragName: Name)
  case UnusedVariable(varName: Name)
  case NonImplementedInterface(interface: NamedType)
  case TypeMismatch(expected: Type, actual: Type)
  case SelfImplementation(tpe: NamedType)
  // case ContainsCycles(name: Name)

  case StructureEmpty(context: Option[String] = None)
  case MissingName(name: Name, context: Option[String] = None)
  case MissingNames(names: List[Name], context: Option[String] = None)
  case MissingImplementations(parent: Name, missing: List[Name], context: Option[String] = None)
  case MissingArgument2(name: Name, context: Option[String] = None)
  case MissingField2(name: Name, parentType: NamedType, context: Option[String] = None)
  case MissingSelection2(fieldName: Name, parentType: NamedType, context: Option[String] = None)
  case MissingVariable2(name: Name, context: Option[String] = None)
  case MissingDefinition(name: Name, context: Option[String] = None)
  case UnusedDefinition(name: Name, context: Option[String] = None)
  case DuplicateName(name: Name, context: Option[String] = None)
  case InvalidName(name: Name, context: Option[String] = None)
  case InvalidType(`type`: Type, context: Option[String] = None)
  case InvalidNamedType(name: Name, context: Option[String] = None)
  case InvalidSelection(fieldName: Name, namedType: NamedType, context: Option[String] = None)
  case CycleDetected(cycle: Name, context: Option[String] = None)
  case OperationDefinitionError(context: Option[String] = None)
  case SubscriptionHasMultipleRoots2(subName: Option[Name], context: Option[String] = None)

// Copyright (c) 2021 by Oliver Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s

import Type.*

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
  case IllegalSelection(fieldName: Name, namedType: NamedType)
  case IllegalType(tpe: Type, message: Option[String] = None)
  case IllegalName(name: Name)
  case InvalidType(tpe: Type)
  // TODO: Should I rename this to InvalidReturnType or something
  case InvalidImplementation(tpe: Type)
  case DuplicateArgument(argName: Name, fieldName: Name, namedType: NamedType)
  case DuplicateFragmentDefinition(fragName: Name)
  case DuplicateOperationDefinition(opName: Name)
  case DuplicateField(fieldName: Name)
  case DuplicateVariable(varName: Name)
  case DuplicateInterface(interface: Name)
  case FragmentContainsCycles(fragName: Name)
  case MultipleAnonymousQueries
  case AnonymousQueryNotAlone
  case SubscriptionHasMultipleRoots(subName: Option[Name])
  case UnusedFragment(fragName: Name)
  case UnusedVariable(varName: Name)
  case NonImplementedInterface(interface: NamedType)
  case TypeMismatch(expected: Type, actual: Type)

// Copyright (c) 2021 by Oliver Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s

import Type.*

enum GqlError:
  case MissingOperationTypeDefinition(opType: OperationType)
  case MissingTypeDefinition(namedType: NamedType)
  case MissingFragmentDefinition(fragName: Name)
  case MissingField(fieldName: Name, namedType: Option[NamedType] = None)
  case MissingSelection(fieldName: Name, namedType: NamedType)
  case MissingArgumentDefinition(argName: Name, fieldName: Name, namedType: NamedType)
  case MissingArgument(argName: Name, fieldName: Name, namedType: NamedType)
  case IllegalSelection(fieldName: Name, namedType: NamedType)
  case DuplicateArgument(argName: Name, fieldName: Name, namedType: NamedType)
  case SubscriptionHasMultipleRoots
  case NameNotUnique
  case AnonymousQueryNotAlone

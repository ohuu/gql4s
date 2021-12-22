package gql4s

import Type.*

enum GqlError:
  case MissingOperationTypeDefinition(opType: OperationType)
  case MissingTypeDefinition(namedType: NamedType)
  case MissingFragmentDefinition(fragName: Name)
  case MissingField(fieldName: Name, namedType: Option[NamedType] = None)
  case SubscriptionHasMultipleRoots
  case NameNotUnique
  case AnonymousQueryNotAlone

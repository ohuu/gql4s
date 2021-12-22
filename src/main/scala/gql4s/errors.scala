package gql4s

enum GqlError:
  case MissingOperationTypeDefinition(opType: OperationType)
  case MissingTypeDefinition(typeName: Name)
  case MissingFragmentDefinition(fragName: Name)
  case MissingField(fieldName: Name, typeName: Option[Name] = None)
  case SubscriptionHasMultipleRoots
  case NameNotUnique
  case AnonymousQueryNotAlone

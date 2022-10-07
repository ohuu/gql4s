// Copyright (c) 2021 by Oliver Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s
package errors

import parsing.*
import parsing.Type.*

enum GqlError:
    // Name errors
    case IllegalName(name: Name)

    // Type errors
    case NonInputType(`type`: Type)
    case NonOutputType(`type`: Type)
    case NonObjectType(`type`: Type)
    case TypeNotInvariant(`type`: Type, other: Type)
    case TypeNotCovariant(`type`: Type, other: Type)
    case TypeMismatch(found: Value, expected: Type)
    case IllegalInputType(`type`: Type)

    // Field errors
    case NoFields(typeDef: Name)
    case FieldDefinitionMissing(missingFieldName: Name, onType: Name)
    case FieldMissing(typeDef: Name, missingFieldName: Name)
    case DuplicateFields(fields: List[Name])

    // Fragment errors
    case FragmentDefinitionUnused(fragDef: Name)
    case FragmentDefinitionMissing(frag: Name)
    case FragmentDefinitionHasCyclicalDependency(cycles: List[Name])
    case FragmentDefinitionHasIllegalType(onTypeDef: Name)
    case FragmentDefinitionHasMissingType(missingType: Name)
    case FragmentUsedOnWrongType(`type`: Name, expected: Name)
    case DuplicateFragmentDefinitions(fragDefs: List[Name])

    // Interface errors
    case InterfaceDefinitionImplementsSelf(typeDef: Name)
    case InterfaceDefinitionMissing(`type`: Name)
    case InterfaceImplementationMissing(typeDef: Name, missingImpl: Name)
    case DuplicateInterfaceImpls(impls: List[Name])

    // Input Object errors
    case InputObjectTypeDefinitionHasCyclicalDependency(cycles: List[Name])
    case InputObjectFieldRequired(requiredFieldName: Name, typeDef: Name)
    case InputObjectFieldMissing(missingFieldName: Name, typeDef: Name)

    // Variable & argument errors
    case VariableDefinitionMissing(name: Name)
    case VariableUnused(name: Name)
    case NullValueFound(valueType: Name)
    case IllegalUseOfVariableAsDefaultValue(varName: Name)
    case InputValueDefinitionMissing(typeDef: Name)
    case RequiredArgumentMissing(arg: Name, `def`: Name)
    case ArgumentCannotBeRequired(fieldDef: Name, arg: Name)
    case DuplicateArguments(args: List[Name])
    case DuplicateVariables(vars: List[Name])

    // Selection set errors
    case SelectionSetOnNonObjectLikeType(field: Name, parentType: Type)
    case SelectionSetExpected(field: Name, parentType: Type)

    // Operation errors
    case OperationTypeMissingTypeDefinition(opType: OperationType)
    case MultipleAnonOperationDefinitions
    case AnonOperationDefinitionNotAlone
    case DuplicateOperationDefinitions(opDefs: List[Name])

    // Directive errors
    case DirectiveDefinitionHasCyclicalDependency(cycles: List[Name])
    case DirectiveDefinitionMissing(dirDef: Name)
    case IllegalDirectiveLocation(dirDef: Name, location: DirectiveLocation)
    case DuplicateDirectives(dirs: List[Name])

    // Union type errors
    case NoUnionMembers(unionTypeDef: Name)
    case DuplicateUnionMembers(members: List[Name])

    // Enum type errors
    case NoEnumValues(enumTypeDef: Name)
    case DuplicateEnumValues(values: List[Name])

    // Subscription errors
    case SubscriptionHasMultipleRoots(opDef: Name)

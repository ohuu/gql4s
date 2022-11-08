// Copyright (c) 2021 by Oliver Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s
package validation

import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.collection.mutable.LinkedHashMap
import scala.util.{Failure, Success, Try}

import cats.implicits.*

import errors.*
import errors.GqlError.*
import parsing.*
import parsing.ExecutableDirectiveLocation as EDL
import parsing.OperationType.*
import parsing.Type.*
import parsing.Value.*
import validation.Topo.*

object DocumentValidator:
    /**   - 5.2.3.1 Checks to see if the used fragment has a single root
      */
    // TODO: need to stop introspection fields in subscription's root
    def validateSubscriptionsHaveSingleRoot(using ctx: Context): Validated[List[OperationDefinition]] =
        def hasSingleRoot(frag: InlineFragment | FragmentSpread, fragDefs: List[FragmentDefinition]): Boolean =
            frag match
                case InlineFragment(_, _, selects) => selects.length == 1
                case FragmentSpread(name, _) =>
                    fragDefs.find(_.name == name) match
                        case None       => false
                        case Some(frag) => frag.selectionSet.length == 1

        val namedOps = ctx.opDefs.filter(_.name.text.nonEmpty)
        val subs     = ctx.opDefs.filter(_.operationType == Subscription)

        // try to find a sub with multiple roots
        val multipleRoots = subs.find {
            case OperationDefinition(_, _, _, _, (f: (InlineFragment | FragmentSpread)) :: Nil) =>
                !hasSingleRoot(f, ctx.fragDefs)
            case OperationDefinition(_, _, _, _, selects) => selects.length > 1
        }

        multipleRoots match
            // TODO: Have to use None because OperationDefinition may not have a name and is actually split into
            // two different case classes (OperationDefinition and OperationDefinitionWithName)
            case Some(opDef) => SubscriptionHasMultipleRoots(opDef.name).invalidNec
            case None        => subs.validNec
    end validateSubscriptionsHaveSingleRoot

    /**   - 5.5.1.2 Fragment types should exist.
      *   - 5.5.1.3 Fragments must reference either Union, Interface or Object types.
      */
    // TODO: Spec contains error - formal spec explicitly mentions named spreads which implies
    //       inlined fragments are not covered by this validation rule, but they clearly are!
    // TODO: This has the potential of being called multiple times for the same type. This can be
    //       optimised by checking for the existence (and usage) of types before the main "sweep".
    def validateFragmentTypeExists(namedType: NamedType)(using ctx: Context): Validated[NamedType] =
        ctx.getTypeDef(namedType.name) match
            case Some(_: ObjectLikeTypeDefinition | _: UnionTypeDefinition) => namedType.validNec
            case Some(typeDef) => FragmentDefinitionHasIllegalType(namedType.name).invalidNec
            case None          => FragmentDefinitionHasMissingType(namedType.name).invalidNec

    def validateVariable(opDef: OperationDefinition)(variable: Variable, expectedType: Type)(using
        ctx: Context
    ): Validated[Value] =
        ctx.getVarDef(opDef)(variable.name) match
            case None => VariableDefinitionMissing(variable.name).invalidNec
            case Some(varDef) =>
                if varDef.`type` == expectedType then variable.validNec
                else TypeMismatch(variable, expectedType).invalidNec

    def skipVariableValidation(variable: Variable, expectedType: Type): Validated[Value] = variable.validNec

    /** Performs various validation steps on selection sets. Bear in mind that this function will not validate fragment
      * definitions but will validate inline fragment definitions, you must call validateFragmentDefinition as well as
      * this function to fully validate an executable document.
      *
      * @param selectionSet
      *   The selectionSet to recursively validate.
      * @param parentType
      *   The name of the type that the selectionSet is within.
      * @param doc
      *   The query document.
      * @param schema
      *   The graphql schema.
      * @return
      *   Return a list of errors or Nil if there weren't any.
      */
    def validateSelectionSet(
        selectionSet: List[Selection],
        parentType: NamedType,
        opDef: Option[OperationDefinition] = None
    )(using
        ctx: Context
    ): Validated[List[Selection]] =
        import ctx.given

        @tailrec
        def recurse(
            accSelectionSet: List[(NamedType, Selection)],
            acc: Validated[List[Selection]] = Nil.validNec
        ): Validated[List[Selection]] =
            accSelectionSet match
                case Nil => acc
                case (parentType, selection) :: tail =>
                    selection match
                        case field @ Field(_, fieldName, arguments, dirs, selectionSet) =>
                            val validatedDirectives = validateDirectives(dirs, EDL.FIELD)

                            // 5.3.1 field selections must exist on object, interface and union types
                            ctx.getFieldDef(parentType)(fieldName) match
                                case None =>
                                    (
                                        FieldDefinitionMissing(fieldName, parentType.name).invalidNec,
                                        validatedDirectives,
                                        acc
                                    )
                                        .mapN((_, _, _) => selectionSet)
                                case Some(fieldDef) =>
                                    val args = field.arguments
                                    val validations = (
                                        validatedDirectives,

                                        // if we're in an operation definition then validation variable references
                                        // for values. Else skip this step because we must be in a fragment def
                                        opDef match
                                            case None        => validateArgs(args, fieldDef)(skipVariableValidation)
                                            case Some(opDef) => validateArgs(args, fieldDef)(validateVariable(opDef))
                                    ).mapN((_, _) => selectionSet)

                                    val fieldType: NamedType = NamedType(fieldDef.`type`.name)
                                    ctx.getTypeDef(fieldType.name) match
                                        case None =>
                                            val validatedDef = FragmentDefinitionMissing(fieldType.name).invalidNec
                                            recurse(tail, validations combine validatedDef combine acc)

                                        // We found it but it's a leaf type so we can't recurse into its children,
                                        // instead just carry on with the parent types other selections (tail)
                                        case Some(_: ScalarTypeDefinition | _: EnumTypeDefinition) =>
                                            // 5.3.3 leaf field selection validation
                                            if selectionSet.isEmpty then recurse(tail, validations combine acc)
                                            else
                                                val validatedSelection =
                                                    SelectionSetOnNonObjectLikeType(fieldName, parentType).invalidNec
                                                recurse(tail, validations combine validatedSelection combine acc)

                                        // We found an object type so we need to recurse
                                        case Some(typeDef) =>
                                            // 5.3.3 leaf field selection validation
                                            if selectionSet.isEmpty then
                                                val validatedSelection =
                                                    SelectionSetExpected(fieldName, parentType).invalidNec
                                                validations combine validatedSelection combine acc
                                            else
                                                val typeAndSelection = selectionSet.map(NamedType(typeDef.name) -> _)
                                                recurse(typeAndSelection ::: tail, validations combine acc)

                        case InlineFragment(Some(onType), dirs, selectionSet) =>
                            val validations = (
                                validateFragmentTypeExists(onType),
                                validateDirectives(dirs, EDL.INLINE_FRAGMENT)
                            ).mapN((_, _) => selectionSet)
                            val typeAndSelection = selectionSet.map(onType -> _).toList

                            // 5.5.2.3.1 Fragment spread type is valid if it's the same type as the parent type (same scope)
                            // 5.5.2.3.2
                            // 5.5.2.3.3
                            val isSame = onType == parentType
                            val isImplementation =
                                validateCovariant(parentType, onType).isValid ||
                                    validateCovariant(onType, parentType).isValid
                            if !(isSame || isImplementation) then
                                FragmentUsedOnWrongType(parentType.name, onType.name).invalidNec
                            else recurse(typeAndSelection ::: tail, validations combine acc)

                        // Type name has been omitted so this inline fragment has the same type as enclosing
                        // context (e.g. the current parentType)
                        case InlineFragment(None, dirs, selectionSet) =>
                            val validations = (
                                validateFragmentTypeExists(parentType),
                                validateDirectives(dirs, EDL.INLINE_FRAGMENT)
                            ).mapN((_, _) => selectionSet)
                            val typeAndSelection = selectionSet.map(parentType -> _).toList

                            // no need for 5.5.2.3.x because in the case that no type is given, by definition
                            // the type of the fragment IS the type of the parent and therefore 5.5.2.3.1 is
                            // satisfied.

                            recurse(typeAndSelection ::: tail, validations combine acc)

                        // Fragment definitions have already been validated by this point so you only need to
                        // check if the fragment definition exists, there's no need to step into the defintion
                        case FragmentSpread(name, dirs) =>
                            ctx.getFragDef(name) match
                                case None =>
                                    // 5.5.2.1 Fragment definition must exist
                                    val validatedFragDef = FragmentDefinitionMissing(name).invalidNec
                                    recurse(tail, validatedFragDef combine acc)

                                case Some(fragDef) =>
                                    // 5.5.2.3.1 Fragment spread must be same type as parent type (same scope)
                                    // 5.5.2.3.2
                                    // 5.5.2.3.3
                                    // TODO: Factor this into it's own function
                                    val onType = fragDef.on
                                    val isSame = onType == parentType
                                    val isImplementation =
                                        validateCovariant(parentType, onType).isValid ||
                                            validateCovariant(onType, parentType).isValid
                                    if !(isSame || isImplementation) then
                                        FragmentUsedOnWrongType(parentType.name, onType.name).invalidNec
                                    else recurse(tail, acc)
        end recurse

        (
            recurse(selectionSet.map(parentType -> _)),
            fieldsInSetCanMerge(selectionSet, parentType)
        ).mapN((validSelectionSet, _) => validSelectionSet)
    end validateSelectionSet

    def validateAnonymousOperationDefinition(
        namedOps: List[OperationDefinition],
        anonOps: List[OperationDefinition]
    ): Validated[Option[OperationDefinition]] =
        if anonOps.length > 1 then MultipleAnonOperationDefinitions.invalidNec
        else if anonOps.length == 1 && !namedOps.isEmpty then AnonOperationDefinitionNotAlone.invalidNec
        else anonOps.headOption.validNec
    end validateAnonymousOperationDefinition

    def validateVariableDefinition(varDef: VariableDefinition)(using
        ctx: Context
    ): Validated[VariableDefinition] =
        import ctx.given

        // 5.8.2 variable type must be an input type
        val validatedVariableTypes = validateInputType(varDef.`type`)
        // if isInputType(varDef.`type`) then varDef.validNec
        // else InvalidType(varDef.`type`).invalidNec

        val validatedDirectives = validateDirectives(varDef.directives, EDL.VARIABLE_DEFINITION)

        val validatedDefaultValue =
            varDef.defaultValue.traverse {
                validateValue(_, varDef.`type`) { (variable, _) =>
                    IllegalUseOfVariableAsDefaultValue(variable.name).invalidNec
                }
            }

        (
            validatedVariableTypes,
            validatedDirectives,
            validatedDefaultValue
        ).mapN((_, _, _) => varDef)
    end validateVariableDefinition

    def validateVariableDefinitions(
        varDefs: List[VariableDefinition],
        requiredVars: Set[Name]
    )(using Context): Validated[List[VariableDefinition]] =
        // 5.8.1 unique variables
        val validatedVariableNames = validateUniqueName(varDefs)(DuplicateVariables(_))

        // 5.8.3 variable uses defined
        val validatedVariablesDefined =
            requiredVars
                .map { varName =>
                    if varDefs.exists(_.name == varName) then ().validNec
                    else VariableDefinitionMissing(varName).invalidNec
                }
                .reduceOption(_ combine _)
                .getOrElse(().validNec)

        // 5.8.4 all variables used
        val validatedVariablesUsed =
            varDefs.traverse { varDef =>
                if requiredVars.exists(_ == varDef.name) then ().validNec
                else VariableUnused(varDef.name).invalidNec
            }

        // 5.8.2 variable type must be an input type
        val validatedVariableDefs = varDefs.traverse(validateVariableDefinition)

        (
            validatedVariableNames,
            validatedVariablesDefined,
            validatedVariablesUsed,
            validatedVariableDefs
        ).mapN((_, _, _, _) => varDefs)
    end validateVariableDefinitions

    def validateOperationDefinition(opDef: OperationDefinition)(using
        ctx: Context
    ): Validated[OperationDefinition] =
        import ctx.given

        val opDefReqs = ctx.getVarReqs(opDef.name).getOrElse(Set.empty[Variable]).map(_.name)

        val validatedVariableDefs = validateVariableDefinitions(opDef.variableDefinitions, opDefReqs)
        val validatedSelectionSets = ctx.getOpTypeDef(opDef.operationType) match
            case Some(typeDef: ObjectTypeDefinition) =>
                validateSelectionSet(opDef.selectionSet.toList, NamedType(typeDef.name), Some(opDef))
            case _ => OperationTypeMissingTypeDefinition(opDef.operationType).invalidNec
        val validatedDirectives = opDef.operationType match
            case Query        => validateDirectives(opDef.directives, EDL.QUERY)
            case Mutation     => validateDirectives(opDef.directives, EDL.MUTATION)
            case Subscription => validateDirectives(opDef.directives, EDL.SUBSCRIPTION)

        (validatedDirectives combine validatedVariableDefs).andThen(_ => validatedSelectionSets).map(_ => opDef)
    end validateOperationDefinition

    def validateOperationDefinitions(opDefs: List[OperationDefinition])(using
        Context
    ): Validated[List[OperationDefinition]] =
        val namedOps = opDefs.filter(_.name.text.nonEmpty)
        val anonOps  = opDefs.filter(_.name.text.isEmpty)

        val validatedOpDefs = opDefs.traverse(validateOperationDefinition)

        (
            // 5.2.1.1 unique operation names
            validateUniqueName(opDefs)(DuplicateOperationDefinitions(_)),

            // 5.2.2.1 Lone anonymous operation
            validateAnonymousOperationDefinition(namedOps, anonOps),

            // validate each operation definition
            validatedOpDefs
        ).mapN((_, _, validatedOpDefs) => opDefs)
    end validateOperationDefinitions

    def validateFragmentDefinition(
        fragDef: FragmentDefinition
    )(using ctx: Context): Validated[FragmentDefinition] =
        import ctx.given
        (
            validateSelectionSet(fragDef.selectionSet.toList, fragDef.on),
            validateFragmentTypeExists(fragDef.on),
            validateDirectives(fragDef.directives, EDL.FRAGMENT_DEFINITION)
        ).mapN((_, _, _) => fragDef)

    def validateAllFragmentsUsed(
        defs: List[FragmentDefinition],
        refs: List[Name]
    ): Validated[List[FragmentDefinition]] =
        defs
            .traverse { fragDef =>
                if refs.contains(fragDef.name) then fragDef.validNec
                else FragmentDefinitionUnused(fragDef.name).invalidNec
            }

    def validateFragmentDefinitions(fragDefs: List[FragmentDefinition])(using
        ctx: Context
    ): Validated[List[FragmentDefinition]] =
        // 5.5.2.2 Fragment definitions must not contain cycles
        // topologically sorting the fragment dependency graph will find cycles and provide an order to
        // find fragment definition requirements
        ctx.fragDeps.topo match
            case HasCycles(cycles) => FragmentDefinitionHasCyclicalDependency(cycles).invalidNec
            case NoCycles(order)   =>
                // TODO: factor this out to a function named findAllFragmentUses
                //       pre-process the sortedgraph (sortedGraph.map ...) instead of doing it each iteration

                val fragmentUses = ctx.fragSpreads
                    .flatMap { name =>
                        val deps = ctx.fragDeps.deps.get(name).getOrElse(Set.empty)
                        name :: deps.toList
                    }

                val allRefsToFragDefs = ctx.fragDeps.deps
                    .map((_, deps) => deps)
                    .foldLeft(Set.empty[Name])(_ union _)
                    .toList

                val validateFragDefsExist = allRefsToFragDefs.traverse { depName =>
                    if ctx.getFragDef(depName).isDefined then depName.validNec
                    else FragmentDefinitionMissing(depName).invalidNec
                }

                (
                    // 5.5.1.1 fragment definition unique name
                    validateUniqueName(fragDefs)(DuplicateFragmentDefinitions(_)),

                    // 5.5.1.4 Fragment definitions must be used
                    validateAllFragmentsUsed(fragDefs, fragmentUses),

                    // 5.5.2.1 Fragment definition must exist
                    validateFragDefsExist,

                    // validate individual frag defs
                    fragDefs.traverse(validateFragmentDefinition)
                ).mapN((_, _, _, _) => fragDefs)
    end validateFragmentDefinitions

    def validate(doc: ExecutableDocument, schemaCtx: SchemaContext): Validated[ExecutableDocument] =
        val docCtx = DocumentContext(doc)

        given Context = Context(schemaCtx, docCtx)

        // 5.1.1
        // This is implied by the fact that the validate function expects doc to be an ExecutableDocument

        validateFragmentDefinitions(docCtx.fragDefs).andThen { _ =>
            (
                validateOperationDefinitions(docCtx.opDefs),
                validateSubscriptionsHaveSingleRoot
            ).mapN((_, _) => doc)
        }
    end validate

    // 5.3.2
    type MergableFields      = (Field, Field)
    type FieldWithParentType = (Field, NamedType)

    // TODO: make this tail recursive!
    def fieldsInSetCanMerge(set: List[Selection], parentType: NamedType)(using
        ctx: Context
    ): Validated[List[MergableFields]] =
        def collectFields(set: List[Selection]): List[Field] =
            set.flatMap {
                case field @ Field(alias, name, _, _, _) => Some(field)
                case _                                   => None
            }

        def getFieldsForName(set: List[Selection], parentType: NamedType): List[FieldWithParentType] = set.flatMap {
            case field: Field => List(field -> parentType)
            case FragmentSpread(name, _) =>
                ctx.getFragDef(name)
                    .map((fragDef) => collectFields(fragDef.selectionSet).map(f => f -> fragDef.on))
                    .getOrElse(Nil)
            case InlineFragment(tpe, _, selectionSet) => collectFields(selectionSet).map(_ -> tpe.getOrElse(parentType))
        }

        def getMergeCandidates(
            set: List[Selection],
            parentType: NamedType
        ): List[(FieldWithParentType, FieldWithParentType)] =
            getFieldsForName(set, parentType)
                .groupBy {
                    case (Field(Some(alias), _, _, _, _), _) => alias
                    case (Field(_, name, _, _, _), _)        => name
                }
                // .groupBy(_._1.name)
                .toList
                .flatMap {
                    case (_, occ) if occ.length > 1 => occ.combinations(2).map(f => (f(0), f(1)))
                    case _                          => Nil
                }

        def isScalarOrEnum(`type`: NamedType): Boolean =
            ctx.getEnumTypeDef(`type`.name).isDefined || ctx.getScalarTypeDef(`type`.name).isDefined

        def isCompositeType(`type`: NamedType): Boolean = ctx.getObjLikeTypeDef(`type`.name).isDefined

        def sameResponseShape(a: FieldWithParentType, b: FieldWithParentType): Boolean =
            val (fieldA, parentTypeA) = a
            val (fieldB, parentTypeB) = b

            def recurse(typeA: Type, typeB: Type): Boolean =
                (typeA, typeB) match
                    case (NonNullType(a), NonNullType(b))          => recurse(a, b)
                    case (NonNullType(_), _) | (_, NonNullType(_)) => false
                    case (ListType(a), ListType(b))                => recurse(a, b)
                    case (ListType(_), _) | (_, ListType(_))       => false
                    case (a: NamedType, b: NamedType) =>
                        if isScalarOrEnum(a) || isScalarOrEnum(b) then a == b
                        else if isCompositeType(a) && isCompositeType(b) then
                            val mergedSet       = fieldA.selectionSet ++ fieldB.selectionSet
                            val mergeCandidates = getMergeCandidates(mergedSet, parentType)
                            mergeCandidates.forall(sameResponseShape)
                        else false

            val typeA = ctx.getFieldDef(parentTypeA)(fieldA.name).get.`type` // must have a field def if we got here
            val typeB = ctx.getFieldDef(parentTypeB)(fieldB.name).get.`type` // must have a field def if we got here
            recurse(typeA, typeB)
        end sameResponseShape

        def isObjectType(field: Field, parentType: NamedType): Boolean =
            ctx.getFieldDef(parentType)(field.name)
                .flatMap(fieldDef => ctx.getObjTypeDef(fieldDef.`type`.name))
                .isDefined

        val mergeCandidates = getMergeCandidates(set, parentType)

        mergeCandidates
            .traverse { case (a @ (fieldA, parentTypeA), b @ (fieldB, parentTypeB)) =>
                if sameResponseShape(a, b) then
                    val sharedParentType = if parentTypeA == parentTypeB then Some(unwrapType(parentTypeA)) else None
                    val eitherNonObject  = isObjectType(fieldA, parentTypeA) || isObjectType(fieldB, parentTypeB)

                    if sharedParentType.isDefined || eitherNonObject then
                        val sameNames = fieldA.name == fieldB.name
                        val sameArgs  = fieldA.arguments.toSet == fieldB.arguments.toSet

                        if sameNames && sameArgs then
                            val mergedSet = (fieldA.selectionSet.toSet union fieldB.selectionSet.toSet).toList
                            if mergedSet.isEmpty then ((fieldA, fieldB) :: Nil).validNec
                            else fieldsInSetCanMerge(mergedSet, sharedParentType.get)
                        else NonMergableFieldsFound((fieldA.name, fieldB.name) :: Nil).invalidNec
                    else ((fieldA, fieldB) :: Nil).validNec
                else NonMergableFieldsFound((fieldA.name, fieldB.name) :: Nil).invalidNec
            }
            .map(_.flatten)
    end fieldsInSetCanMerge

    // def validateVariableUsage(variable: Variable, usage: Argument | InputValueDefinition)(using ctx: Context): Validated[OperationDefinition] =

end DocumentValidator

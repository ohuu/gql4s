// Copyright (c) 2021 by Oliver Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s
package validation

import cats.data.NonEmptyList
import cats.implicits.*
import scala.collection.immutable.HashMap
import scala.annotation.tailrec

import errors.*
import errors.GqlError.*
import parsing.*
import parsing.OperationType.*
import parsing.Type.*
import parsing.Value.*
import scala.collection.mutable.LinkedHashMap

import ExecutableDirectiveLocation as EDL

object DocumentValidator:
  /**
   * A table mapping definition names to the set of variables required to execute that definition.
   */
  type FragDefReqsTable = Map[Name, Set[Variable]]

  // /////////////////////////////////////////////////////////////////////////////////////////////////
  // Helper Functions
  /** Builds a table of variables required to execute the given fragment definition */
  def findFragDefReqs(
      fragDef: FragmentDefinition
      // depReqs: FragDefReqsTable
  )(using doc: ExecutableDocument): FragDefReqsTable =
    @tailrec
    def recurse(
        accSelectionSets: List[(Name, Selection)],
        accReqs: FragDefReqsTable = Map.empty
    ): FragDefReqsTable =
      accSelectionSets match
        case Nil => accReqs

        case (fragDefName, selection) :: tail =>
          selection match
            case field: Field =>
              val vars = field.arguments.flatMap {
                case Argument(_, v: Variable) => Some(v)
                case _                        => None
              }.toSet
              val accSelectionSet = field.selectionSet.map(fragDefName -> _)
              val reqs            = fragDefName -> (vars union accReqs(fragDefName))
              recurse(accSelectionSet ::: tail, accReqs + reqs)

            case frag: InlineFragment =>
              val accSelectionSet = frag.selectionSet.toList.map(fragDefName -> _)
              recurse(accSelectionSet ::: tail, accReqs)

            case frag: FragmentSpread =>
              doc.findFragDef(frag.name) match
                case None =>
                  // Although fragment definition existence has been checked by this point we must
                  // still handle the condition where None is found because validation does not stop
                  // premeturely if some fragments are found to not exist.
                  recurse(tail, accReqs)

                case Some(fragDef) =>
                  val accSelectionSet = fragDef.selectionSet.toList.map(fragDefName -> _)
                  recurse(
                    accSelectionSet ::: tail,
                    accReqs + (fragDef.name -> Set.empty /*depReqs.getOrElse(fragDef.name, Set.empty)*/ )
                  )
          end match
      end match
    end recurse

    val selectionSet           = fragDef.selectionSet.toList.map(fragDef.name -> _)
    val reqs: FragDefReqsTable = Map(fragDef.name -> Set.empty)
    recurse(selectionSet, reqs)
  end findFragDefReqs

  def findOpDefReqs(opDef: OperationDefinition, fragDefReqs: FragDefReqsTable): Set[Variable] =
    def requiredVars(args: List[Argument]): Set[Variable] = args.flatMap {
      case Argument(_, v: Variable) => Some(v)
      case _                        => None
    }.toSet

    @tailrec
    def recurse(
        accSelectionSets: List[Selection],
        accReqs: Set[Variable] = Set.empty
    ): Set[Variable] =
      accSelectionSets match
        case Nil => accReqs

        case (field: Field) :: tail =>
          val allArgs = field.directives.flatMap(_.arguments) ++ field.arguments
          val vars    = requiredVars(allArgs)
          recurse(field.selectionSet ::: tail, vars union accReqs)

        case (frag: InlineFragment) :: tail =>
          val vars = requiredVars(frag.directives.flatMap(_.arguments))
          recurse(frag.selectionSet.toList ::: tail, vars union accReqs)

        case (frag: FragmentSpread) :: tail =>
          val vars = requiredVars(frag.directives.flatMap(_.arguments))
          if fragDefReqs.contains(frag.name) then
            recurse(tail, vars union fragDefReqs(frag.name) union accReqs)
          else recurse(tail, vars union accReqs)

      end match
    end recurse

    recurse(opDef.selectionSet.toList)
  end findOpDefReqs

  def buildFragDefDepGraph(
      fragDefs: List[FragmentDefinition]
  ): DependencyGraph[FragmentDefinition] =
    @tailrec
    def recurse(accSelectionSet: List[Selection], accDeps: Set[Name] = Set.empty): Set[Name] =
      accSelectionSet match
        case Nil                            => accDeps
        case (field: Field) :: tail         => recurse(field.selectionSet ::: tail, accDeps)
        case (frag: InlineFragment) :: tail => recurse(frag.selectionSet.toList ::: tail, accDeps)
        case (frag: FragmentSpread) :: tail => recurse(tail, accDeps + frag.name)
      end match
    end recurse

    // A mapping from fragment defintion to its dependencies
    if fragDefs.nonEmpty then
      LinkedHashMap.from( // maintains insertion order 👍
        fragDefs
          .map(fragDef =>
            val deps = recurse(fragDef.selectionSet.toList)
            fragDef -> deps
          )
      )
    else LinkedHashMap.empty
  end buildFragDefDepGraph

  // /////////////////////////////////////////////////////////////////////////////////////////////////
  // Validators

  /**
   *   - 5.2.3.1 Checks to see if the used fragment has a single root
   */
  // TODO: need to stop introspection fields in subscription's root
  def validateSubscriptionsHaveSingleRoot(using
      doc: ExecutableDocument
  ): Validated[List[OperationDefinition]] =
    def hasSingleRoot(
        frag: InlineFragment | FragmentSpread,
        fragDefs: List[FragmentDefinition]
    ): Boolean =
      frag match
        case InlineFragment(_, _, selects) => selects.length == 1
        case FragmentSpread(name, _) =>
          fragDefs.find(_.name == name) match
            case None       => false
            case Some(frag) => frag.selectionSet.length == 1
    end hasSingleRoot

    val ops   = doc.getDef[OperationDefinition]
    val frags = doc.getDef[FragmentDefinition]

    val namedOps = ops.filter(_.name.text.nonEmpty)
    val subs     = ops.filter(_.operationType == Subscription)

    // try to find a sub with multiple roots
    val multipleRoots = subs.find {
      case OperationDefinition(
             _,
             _,
             _,
             _,
             NonEmptyList(f: (InlineFragment | FragmentSpread), Nil)
           ) =>
        !hasSingleRoot(f, frags)
      case OperationDefinition(_, _, _, _, selects) => selects.length > 1
    }

    multipleRoots match
      // TODO: Have to use None because OperationDefinition may not have a name and is actually split into
      // two different case classes (OperationDefinition and OperationDefinitionWithName)
      case Some(opDef) => SubscriptionHasMultipleRoots(None).invalidNec
      case None        => subs.validNec
  end validateSubscriptionsHaveSingleRoot

  /**
   *   - 5.5.1.2 Fragment types should exist.
   *   - 5.5.1.3 Fragments must reference either Union, Interface or Object types.
   */
  // TODO: Spec contains error - formal spec explicitly mentions named spreads which implies
  //       inlined fragments are not covered by this validation rule, but they clearly are!
  // TODO: This has the potential of being called multiple times for the same type. This can be
  //       optimised by checking for the existence (and usage) of types before the main "sweep".
  def validateObjectLikeTypeDefExists(
      namedType: NamedType
  )(using schema: TypeSystemDocument): Validated[NamedType] =
    schema.findTypeDef[TypeDefinition](namedType.name) match
      case Some(_: ObjectTypeDefinition | _: InterfaceTypeDefinition | _: UnionTypeDefinition) =>
        namedType.validNec
      case Some(typeDef) => InvalidNamedType(typeDef.name).invalidNec
      case None          => MissingDefinition(namedType.name).invalidNec
  end validateObjectLikeTypeDefExists

  /**
   *   - 5.6.2 input object field exists
   *   - 5.6.3 input object field duplicates
   *   - 5.6.4 input objects required fields
   */
  def validateInputObjectValue(
      inObjVal: ObjectValue,
      inValDef: InputValueDefinition
  )(using schema: TypeSystemDocument): Validated[ObjectValue] =
    @tailrec
    def recurse(
        inObjs: List[(ObjectField, InputObjectTypeDefinition)],
        acc: Validated[Unit]
    ): Validated[Unit] =
      inObjs match
        case Nil => acc

        case (ObjectField(name, value: ObjectValue), inObjTypeDef) :: tail =>
          // 5.6.3 input object field duplicates
          val validatedFieldNames = validateUniqueName(value.fields).map(_ => ())

          // 5.6.2 input object field exists
          inObjTypeDef.fields.find(_.name == name) match
            case None =>
              val parentType: NamedType = NamedType(inObjTypeDef.name)
              val validatedMissingField = MissingField2(name, parentType).invalidNec
              recurse(tail, validatedFieldNames combine validatedMissingField combine acc)
            case Some(inValDef) =>
              schema.findTypeDef[InputObjectTypeDefinition](inValDef.`type`.name) match
                case None =>
                  val validatedMissingDef = MissingDefinition(inValDef.`type`.name).invalidNec
                  recurse(tail, validatedFieldNames combine validatedMissingDef combine acc)
                case Some(inObjTypeDef) =>
                  // 5.6.4 input objects required fields
                  val validatedRequiredFields = inObjTypeDef.fields
                    .filter(_.`type`.isInstanceOf[NonNullType])
                    .traverse(inputValDef =>
                      value.fields.find(_.name == inputValDef.name) match
                        case None =>
                          MissingField2(inputValDef.name, NamedType(inObjTypeDef.name))
                            .invalidNec[Unit]
                        case Some(_) => ().validNec[GqlError]
                    )
                    .map(_ => ())

                  recurse(
                    value.fields.map(_ -> inObjTypeDef) ::: tail,
                    validatedFieldNames combine validatedRequiredFields combine acc
                  )

        case (ObjectField(name, value), inObjTypeDef) :: tail =>
          // 5.6.2 input object field exists
          inObjTypeDef.fields.find(_.name == name) match
            case None =>
              val validatedMissingField =
                MissingField2(name, NamedType(inObjTypeDef.name)).invalidNec
              recurse(tail, validatedMissingField combine acc)
            case Some(_) => recurse(tail, acc)
    end recurse

    // 5.6.3 input object field duplicates
    val validatedFieldNames = validateUniqueName(inObjVal.fields).map(_ => ())

    val validatedInObjVal =
      schema.findTypeDef[InputObjectTypeDefinition](inValDef.`type`.name) match
        case None =>
          val validatedTypeDef = MissingDefinition(inValDef.`type`.name).invalidNec[Unit]
          validatedFieldNames combine validatedTypeDef
        case Some(inObjTypeDef) =>
          // 5.6.4 input objects required fields
          val validatedRequiredFields = inObjTypeDef.fields
            .filter(_.`type`.isInstanceOf[NonNullType])
            .map(_.name)
            .map(name =>
              inObjVal.fields.find(_.name == name) match
                case None    => MissingField2(name, NamedType(inObjTypeDef.name)).invalidNec
                case Some(_) => ().validNec
            )
            .reduceLeft(_ combine _)

          recurse(
            inObjVal.fields.map(_ -> inObjTypeDef),
            validatedFieldNames combine validatedRequiredFields
          )

    validatedInObjVal.map(_ => inObjVal)
  end validateInputObjectValue

  /**
   * Performs various validation steps on selection sets. Bear in mind that this function will not
   * validate fragment definitions but will validate inline fragment definitions, you must call
   * validateFragmentDefinition as well as this function to fully validate an executable document.
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
  private def validateSelectionSet(
      selectionSet: List[Selection],
      parentType: NamedType
  )(using schema: TypeSystemDocument, doc: ExecutableDocument): Validated[List[Selection]] =
    @tailrec
    def recurse(
        accSelectionSet: List[(NamedType, Selection)],
        acc: Validated[Unit]
    ): Validated[Unit] =
      accSelectionSet match
        case Nil => acc
        case (parentType, selection) :: tail =>
          selection match
            case field @ Field(_, fieldName, arguments, dirs, selectionSet) =>
              // 5.3.1 field selections must exist on object, interface and union types
              schema.findFieldDef(fieldName, parentType) match
                case None =>
                  val validatedField      = MissingField2(fieldName, parentType).invalidNec[Unit]
                  val validatedDirectives = validateDirectives(dirs, EDL.FIELD).map(_ => ())
                  validatedField combine validatedDirectives combine acc
                case Some(fieldDef) =>
                  val validatedArgs =
                    validateArguments(field.arguments, fieldDef).map(_ => ())
                  val validatedDirectives = validateDirectives(dirs, EDL.FIELD).map(_ => ())

                  val fieldType: NamedType = NamedType(fieldDef.`type`.name)
                  schema.findTypeDef[TypeDefinition](fieldType.name) match
                    case None =>
                      val validatedDef = MissingTypeDefinition(fieldType).invalidNec
                      recurse(
                        tail,
                        validatedArgs combine validatedDirectives combine validatedDef combine acc
                      )

                    // We found it but it's a leaf type so we can't recurse into its children,
                    // instead just carry on with the parent types other selections (tail)
                    case Some(_: ScalarTypeDefinition | _: EnumTypeDefinition) =>
                      // 5.3.3 leaf field selection validation
                      if selectionSet.isEmpty then
                        recurse(
                          tail,
                          validatedArgs combine validatedDirectives combine acc
                        )
                      else
                        val validatedSelection = InvalidSelection(fieldName, parentType).invalidNec
                        recurse(
                          tail,
                          validatedArgs combine validatedDirectives combine validatedSelection combine acc
                        )

                    // We found an object type so we need to recurse
                    case Some(typeDef) =>
                      // 5.3.3 leaf field selection validation
                      if selectionSet.isEmpty then
                        val validatedSelection = MissingSelection2(fieldName, parentType).invalidNec
                        validatedArgs combine validatedDirectives combine validatedSelection combine acc
                      else
                        val typeAndSelection = selectionSet.map(NamedType(typeDef.name) -> _)
                        recurse(
                          typeAndSelection ::: tail,
                          validatedArgs combine validatedDirectives combine acc
                        )
                  end match

            case InlineFragment(Some(onType), dirs, selectionSet) =>
              val validatedTypeDef    = validateObjectLikeTypeDefExists(onType).map(_ => ())
              val validatedDirectives = validateDirectives(dirs, EDL.INLINE_FRAGMENT).map(_ => ())
              val typeAndSelection    = selectionSet.map(onType -> _).toList

              // 5.5.2.3.1 Fragment spread type is valid if it's the same type as the parent type (same scope)
              // 5.5.2.3.2
              // 5.5.2.3.3
              val isSame = onType == parentType
              val isImplementation =
                SchemaValidator.validateCovariant(parentType, onType).isValid || SchemaValidator
                  .validateCovariant(onType, parentType)
                  .isValid
              if !(isSame || isImplementation) then InvalidFragment(onType.name).invalidNec
              else
                recurse(
                  typeAndSelection ::: tail,
                  validatedTypeDef combine validatedDirectives combine acc
                )

            // Type name has been omitted so this inline fragment has the same type as enclosing
            // context (e.g. the current parentType)
            case InlineFragment(None, dirs, selectionSet) =>
              val validatedTypeDef    = validateObjectLikeTypeDefExists(parentType).map(_ => ())
              val validatedDirectives = validateDirectives(dirs, EDL.INLINE_FRAGMENT).map(_ => ())
              val typeAndSelection    = selectionSet.map(parentType -> _).toList

              // no need for 5.5.2.3.x because in the case that no type is given, by definition
              // the type of the fragment IS the type of the parent and therefore 5.5.2.3.1 is
              // satisfied.

              recurse(
                typeAndSelection ::: tail,
                validatedTypeDef combine validatedDirectives combine acc
              )

            // Fragment definitions have already been validated by this point so you only need to
            // check if the fragment definition exists, there's no need to step into the defintion
            case FragmentSpread(name, dirs) =>
              doc.findFragDef(name) match
                case None =>
                  // 5.5.2.1 Fragment definition must exist
                  val validatedFragDef = MissingDefinition(name).invalidNec
                  recurse(tail, validatedFragDef combine acc)

                case Some(fragDef) =>
                  // 5.5.2.3.1 Fragment spread must be same type as parent type (same scope)
                  // 5.5.2.3.2
                  // 5.5.2.3.3
                  // TODO: Factor this into it's own function
                  val isSame = fragDef.on == parentType
                  val isImplementation =
                    SchemaValidator.validateCovariant(parentType, fragDef.on).isValid ||
                      SchemaValidator.validateCovariant(fragDef.on, parentType).isValid
                  if !(isSame || isImplementation) then InvalidFragment(fragDef.on.name).invalidNec
                  else recurse(tail, acc)
    end recurse

    recurse(selectionSet.map(parentType -> _), ().validNec).map(_ => selectionSet)
  end validateSelectionSet

  private def validateAnonymousOperationDefinition(
      namedOps: List[OperationDefinition],
      anonOps: List[OperationDefinition]
  ): Validated[Unit] =
    if anonOps.length > 1 then
      OperationDefinitionError(Some("multiple anonymous definitions")).invalidNec
    else if anonOps.length == 1 && !namedOps.isEmpty then
      OperationDefinitionError(Some("Anonymous operation not alone")).invalidNec
    else ().validNec
  end validateAnonymousOperationDefinition

  private def validateVariableDefinition(
      varDef: VariableDefinition
  )(using schema: TypeSystemDocument): Validated[VariableDefinition] =
    // 5.8.2 variable type must be an input type
    val validatedVariableTypes =
      if schema.isInputType(varDef.`type`) then varDef.validNec
      else InvalidType(varDef.`type`).invalidNec

    val validatedDirectives = validateDirectives(varDef.directives, EDL.VARIABLE_DEFINITION)

    val validatedDefaultValue = schema.findTypeDef[TypeDefinition](varDef.`type`.name) match
      case Some(typeDef) => varDef.defaultValue.map(validateValue(_, typeDef)).sequence
      case None          => None.valid

    (
      validatedVariableTypes,
      validatedDirectives,
      validatedDefaultValue
    ).mapN((_, _, _) => varDef)
  end validateVariableDefinition

  private def validateVariableDefinitions(
      varDefs: List[VariableDefinition],
      requiredVars: Set[Variable]
  )(using schema: TypeSystemDocument): Validated[List[VariableDefinition]] =
    // 5.8.1 unique variables
    val validatedVariableNames = validateUniqueName(varDefs)

    // 5.8.3 variable uses defined
    val validatedVariablesDefined =
      requiredVars
        .map(variable =>
          if varDefs.exists(_.name == variable.name) then ().validNec
          else MissingVariable2(variable.name).invalidNec
        )
        .reduceOption(_ combine _)
        .getOrElse(().validNec)

    // 5.8.4 all variables used
    val validatedVariablesUsed =
      varDefs.traverse(varDef =>
        if requiredVars.exists(_.name == varDef.name) then ().validNec
        else UnusedDefinition(varDef.name).invalidNec
      )

    // 5.8.2 variable type must be an input type
    val validatedVariableDefs = varDefs.traverse(validateVariableDefinition)

    (
      validatedVariableNames,
      validatedVariablesDefined,
      validatedVariablesUsed,
      validatedVariableDefs
    ).mapN((_, _, _, _) => varDefs)
  end validateVariableDefinitions

  private def validateOperationDefinition(
      opDef: OperationDefinition,
      fragDefReqs: FragDefReqsTable
  )(using
      schema: TypeSystemDocument,
      doc: ExecutableDocument
  ): Validated[OperationDefinition] =
    val opDefReqs = findOpDefReqs(opDef, fragDefReqs)

    val validatedVariableDefs = validateVariableDefinitions(opDef.variableDefinitions, opDefReqs)

    val validatedSelectionSets = schema.findOpTypeDef(opDef.operationType) match
      case None =>
        MissingDefinition(Name("")).invalidNec // TODO: Need to handle non named type defs somehow
      case Some(typeDef) =>
        validateSelectionSet(opDef.selectionSet.toList, NamedType(typeDef.name))

    val validatedDirectives = opDef.operationType match
      case Query    => validateDirectives(opDef.directives, EDL.QUERY)
      case Mutation => validateDirectives(opDef.directives, EDL.MUTATION)
      case Subscription =>
        validateDirectives(opDef.directives, EDL.SUBSCRIPTION)

    (
      validatedVariableDefs,
      validatedSelectionSets,
      validatedDirectives
    ).mapN((_, _, _) => opDef)
  end validateOperationDefinition

  private def validateOperationDefinitions(
      fragDefReqs: FragDefReqsTable
  )(using
      doc: ExecutableDocument,
      schema: TypeSystemDocument
  ): Validated[List[OperationDefinition]] =
    val opDefs   = doc.findExecDef[OperationDefinition]
    val namedOps = opDefs.filter(_.name.text.nonEmpty)
    val anonOps  = opDefs.filter(_.name.text.isEmpty)

    val validatedOpDefs =
      opDefs.traverse(validateOperationDefinition(_, fragDefReqs))

    (
      // 5.2.1.1 unique operation names
      validateUniqueName(opDefs),

      // 5.2.2.1 Lone anonymous operation
      validateAnonymousOperationDefinition(namedOps, anonOps),

      // validate each operation definition
      validatedOpDefs
    ).mapN((_, _, validatedOpDefs) => opDefs)
  end validateOperationDefinitions

  private def validateFragmentDefinition(
      fragDef: FragmentDefinition
  )(using doc: ExecutableDocument, schema: TypeSystemDocument): Validated[FragmentDefinition] =
    (
      validateSelectionSet(fragDef.selectionSet.toList, fragDef.on),
      validateObjectLikeTypeDefExists(fragDef.on),
      validateDirectives(fragDef.directives, EDL.FRAGMENT_DEFINITION)
    ).mapN((_, _, _) => fragDef)
  end validateFragmentDefinition

  private def validateFragmentDefinitions(doc: ExecutableDocument)(using
      schema: TypeSystemDocument
  ): Validated[FragDefReqsTable] =
    given ExecutableDocument = doc

    val fragDefs = doc.findExecDef[FragmentDefinition]

    // 5.5.2.2 Fragment definitions must not contain cycles
    // topologically sorting the fragment dependency graph will find cycles and provide an order to
    // find fragment definition requirements
    topologicalSort(buildFragDefDepGraph(fragDefs))
      .andThen(sortedGraph =>

        // TODO: factor this out to a function named findAllFragmentUses
        //       pre-process the sortedgraph (sortedGraph.map ...) instead of doing it each iteration
        val fragmentUses =
          doc
            .findFragSpreads()
            .map(_.name)
            .flatMap(name =>
              name :: sortedGraph.map((k, v) => (k.name, v.toList)).getOrElse(name, Nil)
            )

        val validateFragDefsExist = sortedGraph.toList
          .traverse { case (name, deps) =>
            deps.toList.traverse(depName =>
              if sortedGraph.exists((fragDef, _) => fragDef.name == depName) then depName.validNec
              else MissingDefinition(depName).invalidNec
            )
          }
          .map(_.flatten)

        (
          // 5.5.1.1 fragment definition unique name
          validateUniqueName(fragDefs),

          // 5.5.1.4 Fragment definitions must be used
          validateIsUsed(fragDefs.map(_.name), fragmentUses),

          // 5.5.2.1 Fragment definition must exist
          validateFragDefsExist,

          // validate individual frag defs
          fragDefs.traverse(validateFragmentDefinition)
        ).mapN((_, _, _, _) =>
          if sortedGraph.isEmpty then Map.empty
          else
            val sortedFragDefs = sortedGraph.map((fragDef, _) => fragDef)
            findFragDefReqs(sortedFragDefs.head)
        )
      )
  end validateFragmentDefinitions

  def validate(doc: ExecutableDocument)(using TypeSystemDocument): Validated[ExecutableDocument] =
    given ExecutableDocument = doc

    // 5.1.1
    // This is implied by the fact that the validate function expects doc to be an ExecutableDocument

    validateFragmentDefinitions(doc).andThen(reqs =>
      (
        validateOperationDefinitions(reqs),
        validateSubscriptionsHaveSingleRoot
      ).mapN((_, _) => doc)
    )
  end validate

  // 5.3.2
  // TODO: Can this be integrated into the normal traversal of the document
  // def fieldsInSetCanMerge(
  //     set: SelectionSet,
  //     parentType: Type
  // )(using doc: ExecutableDocument, schema: TypeSystemDocument): Boolean =
  //   def pairs[T](xs: List[T]): List[(T, T)] =
  //     val ps = for
  //       i <- 0 until xs.length - 1
  //       j <- (i + 1) until xs.length
  //     yield (xs(i), xs(j))

  //     ps.toList
  //   end pairs

  //   def getType(s: Selection, parentType: NamedType): Option[Type] = s match
  //     case Field(_, name, _, _, _) =>
  //       schema.findFieldDef(name, parentType).map(_.`type`)
  //     case FragmentSpread(name, _) =>
  //       doc.findFragDef(name).map(_.on)
  //     case InlineFragment(onType, _, _) => onType
  //   end getType

  //   def fragToFieldNameMap(parentType: Type)(
  //       frag: InlineFragment | FragmentSpread
  //   ): Map[Name, List[(Selection, Type)]] =
  //     val selectionSetWithType = frag match
  //       case InlineFragment(None, _, selectionSet) => selectionSet.toList.map(_ -> parentType)
  //       case InlineFragment(Some(onType), _, selectionSet) => selectionSet.toList.map(_ -> onType)
  //       case FragmentSpread(name, _) =>
  //         doc
  //           .findFragDef(name)
  //           .map(fragDef => fragDef.selectionSet.toList.map(_ -> fragDef.on))
  //           .getOrElse(Nil)

  //     selectionSetWithType
  //       .map { case (selectionSet, parentType) =>
  //         selectionToFieldNameMap(parentType)(selectionSet)
  //       }
  //       .reduceOption(_ combine _)
  //       .getOrElse(Map.empty)
  //   end fragToFieldNameMap

  //   // TODO: Make this tail recursive
  //   def selectionToFieldNameMap(parentType: Type)(
  //       selection: Selection
  //   ): Map[Name, List[(Selection, Type)]] = selection match
  //     case field: Field         => Map(field.name -> List((field, parentType)))
  //     case frag: InlineFragment => fragToFieldNameMap(parentType)(frag)
  //     case frag: FragmentSpread => fragToFieldNameMap(parentType)(frag)
  //   end selectionToFieldNameMap

  //   def hasSameResponseShape(selA: Selection, selB: Selection): Boolean =
  //     val typeA = getType(selA)
  //   end hasSameResponseShape

  //   val fieldToNameMap      = set.map(selectionToFieldNameMap(parentType)).reduceLeft(_ combine _)
  //   val duplicateSelections = fieldToNameMap.values.flatMap(pairs).toList

  //   val sameResponseShape = duplicateSelections.map(hasSameResponseShape.tupled)

  //   ???
  // end fieldsInSetCanMerge
end DocumentValidator

// Copyright (c) 2021 by Oliver Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s
package validation

import cats.data.NonEmptyList
import cats.implicits.*
import scala.collection.immutable.HashMap
import scala.annotation.tailrec

import GqlError.*
import OperationType.*
import Selection.*
import Type.*
import Value.*
import scala.collection.mutable.LinkedHashMap

object DocumentValidator:
  /** A table mapping definition names to the set of variables required to execute that definition.
    */
  type FragDefReqsTable = Map[Name, Set[Variable]]

  /** A mapping from fragment defintion to its dependencies */
  type FragDefDepsGraph =
    LinkedHashMap[FragmentDefinition, Set[Name]] // maintains insertion order 👍

  ///////////////////////////////////////////////////////////////////////////////////////////////////
  // Helper Functions
  /** Builds a table of variables required to execute the given fragment definition */
  def findFragDefReqs(
      fragDef: FragmentDefinition,
      depReqs: FragDefReqsTable,
      doc: ExecutableDocument
  ): FragDefReqsTable =
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
                  recurse(tail, accReqs + (fragDef.name.get -> depReqs(fragDef.name.get)))
          end match
      end match
    end recurse

    val selectionSet           = fragDef.selectionSet.toList.map(fragDef.name.get -> _)
    val reqs: FragDefReqsTable = Map(fragDef.name.get -> Set.empty)
    recurse(selectionSet, reqs)
  end findFragDefReqs

  def findOpDefReqs(opDef: OperationDefinition, fragDefReqs: FragDefReqsTable): Set[Variable] =
    @tailrec
    def recurse(
        accSelectionSets: List[Selection],
        accReqs: Set[Variable] = Set.empty
    ): Set[Variable] =
      accSelectionSets match
        case Nil => accReqs

        case (field: Field) :: tail =>
          val vars = field.arguments.flatMap {
            case Argument(_, v: Variable) => Some(v)
            case _                        => None
          }.toSet
          recurse(field.selectionSet ::: tail, vars union accReqs)

        case (frag: InlineFragment) :: tail => recurse(frag.selectionSet.toList ::: tail, accReqs)

        case (frag: FragmentSpread) :: tail =>
          if fragDefReqs.contains(frag.name) then
            recurse(tail, fragDefReqs(frag.name) union accReqs)
          else recurse(tail, accReqs)

      end match
    end recurse

    recurse(opDef.selectionSet.toList)
  end findOpDefReqs

  def buildFragDefDepGraph(fragDefs: List[FragmentDefinition]): FragDefDepsGraph =
    @tailrec
    def recurse(accSelectionSet: List[Selection], accDeps: Set[Name] = Set.empty): Set[Name] =
      accSelectionSet match
        case Nil                            => accDeps
        case (field: Field) :: tail         => recurse(field.selectionSet ::: tail, accDeps)
        case (frag: InlineFragment) :: tail => recurse(frag.selectionSet.toList ::: tail, accDeps)
        case (frag: FragmentSpread) :: tail => recurse(tail, accDeps + frag.name)
      end match
    end recurse

    if fragDefs.nonEmpty then
      LinkedHashMap.from(
        fragDefs
          .map(fragDef =>
            val deps = recurse(fragDef.selectionSet.toList)
            fragDef -> deps
          )
      )
    else LinkedHashMap.empty
  end buildFragDefDepGraph

  // QUESTION: Maybe change this to a while loop?
  def topologicalSort(graph: FragDefDepsGraph): Either[List[GqlError], FragDefDepsGraph] =
    @tailrec
    def recurse(
        graph: FragDefDepsGraph,
        ordering: FragDefDepsGraph = LinkedHashMap.empty
    ): Either[List[GqlError], FragDefDepsGraph] =
      if graph.isEmpty then ordering.asRight
      else
        // find a node with zero inputs
        val zeroDegree =
          graph
            .find((fragDef, _) => graph.find((_, deps) => deps.contains(fragDef.name.get)).isEmpty)

        zeroDegree match
          // 5.5.2.2 Fragment definitions must not contain cycles
          case None =>
            graph.map((fragDef, _) => FragmentContainsCycles(fragDef.name.get)).toList.asLeft
          case Some(zeroDegree @ (zeroDegreeFragDef, _)) =>
            recurse(
              graph.filter((fragDef, _) => fragDef != zeroDegreeFragDef),
              ordering += zeroDegree
            )
    end recurse

    recurse(graph)
  end topologicalSort

  ///////////////////////////////////////////////////////////////////////////////////////////////////
  // Validators

  /**   - 5.2.3.1 Checks to see if the used fragment has a single root
    */
  // TODO: need to stop introspection fields in subscription's root
  def validateSubscriptionsHaveSingleRoot(doc: ExecutableDocument): List[GqlError] =
    def hasSingleRoot(
        frag: InlineFragment | FragmentSpread,
        fragDefs: List[FragmentDefinition]
    ): Boolean =
      frag match
        case InlineFragment(_, _, selects) => selects.length == 1
        case FragmentSpread(name, _) =>
          fragDefs.find(_.name.get == name) match
            case None       => false
            case Some(frag) => frag.selectionSet.length == 1
    end hasSingleRoot

    val ops   = doc.definitions.collect { case x: OperationDefinition => x }
    val frags = doc.definitions.collect { case x: FragmentDefinition => x }
    val subs  = ops.filter(_.operationType == Subscription)

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
      case Some(opDef) => SubscriptionHasMultipleRoots(opDef.name) :: Nil
      case None        => Nil
  end validateSubscriptionsHaveSingleRoot

  /**   - 5.5.1.2 Fragment types should exist.
    *   - 5.5.1.3 Fragments must reference either Union, Interface or Object types.
    */
  // TODO: Spec contains error - formal spec explicitly mentions named spreads which implies
  //       inlined fragments are not covered by this validation rule, but they clearly are!
  def validateObjectLikeTypeDefExists(
      namedType: NamedType,
      schema: TypeSystemDocument
  ): List[GqlError] =
    schema.findTypeDef(namedType) match
      case Some(_: ObjectTypeDefinition | _: InterfaceTypeDefinition | _: UnionTypeDefinition) =>
        Nil
      case Some(typeDef) => IllegalType(NamedType(typeDef.name)) :: Nil
      case None          => MissingTypeDefinition(namedType) :: Nil

  /**   - 5.6.2 input object field exists
    *   - 5.6.3 input object field duplicates
    *   - 5.6.4 input objects required fields
    */
  def validateInputObjectValue(
      inObjVal: ObjectValue,
      inValDef: InputValueDefinition,
      schema: TypeSystemDocument
  ): List[GqlError] =
    @tailrec
    def recurse(
        inObjs: List[(ObjectField, InputObjectTypeDefinition)],
        accErrs: List[GqlError] = Nil
    ): List[GqlError] =
      inObjs match
        case Nil => accErrs

        case (ObjectField(name, value: ObjectValue), inObjTypeDef) :: tail =>
          // 5.6.3 input object field duplicates
          val duplicateErrs = value.fields
            .groupBy(_.name)
            .filter(_._2.length > 1)
            .map { case (name, _) => DuplicateField(name) }
            .toList

          val errs = duplicateErrs ::: accErrs

          // 5.6.2 input object field exists
          inObjTypeDef.fieldsDef.find(_.name == name) match
            case None => recurse(tail, MissingField(name, NamedType(inObjTypeDef.name)) :: errs)
            case Some(inValDef) =>
              schema.findInputObjTypeDef(inValDef.tpe.name) match
                case None =>
                  recurse(tail, MissingInputObjectTypeDefinition(inValDef.tpe.name) :: errs)
                case Some(inObjTypeDef) =>
                  // 5.6.4 input objects required fields
                  val requiredErrs = inObjTypeDef.fieldsDef
                    .filter(_.tpe.isInstanceOf[NonNullType])
                    .map(_.name)
                    .flatMap(name =>
                      value.fields.find(_.name == name) match
                        case None    => List(MissingField(name, NamedType(inObjTypeDef.name)))
                        case Some(_) => Nil
                    )

                  recurse(value.fields.map(_ -> inObjTypeDef) ::: tail, errs)

        case (ObjectField(name, value), inObjTypeDef) :: tail =>
          // 5.6.2 input object field exists
          inObjTypeDef.fieldsDef.find(_.name == name) match
            case None => recurse(tail, MissingField(name, NamedType(inObjTypeDef.name)) :: accErrs)
            case Some(_) => recurse(tail, accErrs)
    end recurse

    // 5.6.3 input object field duplicates
    val duplicateErrs = inObjVal.fields
      .groupBy(_.name)
      .filter(_._2.length > 1)
      .map { case (name, _) => DuplicateField(name) }
      .toList

    schema.findInputObjTypeDef(inValDef.tpe.name) match
      case None => MissingInputObjectTypeDefinition(inValDef.tpe.name) :: duplicateErrs
      case Some(inObjTypeDef) =>
        // 5.6.4 input objects required fields
        val requiredErrs = inObjTypeDef.fieldsDef
          .filter(_.tpe.isInstanceOf[NonNullType])
          .map(_.name)
          .flatMap(name =>
            inObjVal.fields.find(_.name == name) match
              case None    => List(MissingField(name, NamedType(inObjTypeDef.name)))
              case Some(_) => Nil
          )

        recurse(inObjVal.fields.map(_ -> inObjTypeDef), duplicateErrs ::: requiredErrs)
  end validateInputObjectValue

  /** Validate a fields arguments
    * @param field
    *   The field whos arguments we will validate.
    * @param fieldDef
    *   The definition of the field
    * @param parentType
    *   The type of the object containing the field
    */
  def validateArguments(
      field: Field,
      fieldDef: FieldDefinition,
      parentType: NamedType,
      schema: TypeSystemDocument
  ): List[GqlError] =
    // TODO: Choose better name for this
    val argErrors =
      field.arguments
        .flatMap {
          case Argument(name, objVal: ObjectValue) =>
            // 5.4.1 args exist
            // TODO: Include directives in this
            fieldDef.arguments.find(_.name == name) match
              case None => MissingArgumentDefinition(name, field.name, parentType) :: Nil
              case Some(inValDef) =>
                validateInputObjectValue(objVal, inValDef, schema)

          case Argument(name, _) =>
            // 5.4.1 args exist
            // TODO: Include directives in this
            fieldDef.arguments.find(_.name == name) match
              case None => MissingArgumentDefinition(name, field.name, parentType) :: Nil
              case _    => Nil
        }

    // 5.4.2 args are unique
    // TODO: Include directives in this
    val argUniquenessErrors =
      field.arguments
        .groupBy(_.name)
        .filter { case (argName, args) => args.size > 1 }
        .map { case (argName, _) => DuplicateArgument(argName, field.name, parentType) }
        .toList

    // 5.4.2.1 required args
    // TODO: Include directives in this
    val requiredArgs =
      fieldDef.arguments
        .filter {
          case InputValueDefinition(_, _: NonNullType, None, _) => true
          case _                                                => false
        }
        .map(_.name)
    val requiredArgErrors = requiredArgs
      .filter(argName => field.arguments.find(_.name == argName).isEmpty)
      .map(argName => MissingArgument(argName, field.name, parentType))

    argErrors ::: argUniquenessErrors ::: requiredArgErrors
  end validateArguments

  /** Performs various validation steps on selection sets. Bear in mind that this function will not
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
      parentType: NamedType,
      doc: ExecutableDocument,
      schema: TypeSystemDocument
  ): List[GqlError] =
    @tailrec
    def recurse(
        accSelectionSet: List[(NamedType, Selection)],
        accErrs: List[GqlError] = Nil
    ): List[GqlError] =
      accSelectionSet match
        case Nil => accErrs
        case (parentType, selection) :: tail =>
          selection match
            case field @ Field(_, fieldName, arguments, _, selectionSet) =>
              // 5.3.1 field selections must exist on object, interface and union types
              schema.findFieldDef(fieldName, parentType) match
                case None => MissingField(fieldName, parentType) :: accErrs
                case Some(fieldDef) =>
                  val argErrs = validateArguments(field, fieldDef, parentType, schema)
                  val errors  = argErrs ::: accErrs

                  val fieldType: NamedType = NamedType(fieldDef.tpe.name)
                  schema.findTypeDef(fieldType) match
                    case None => recurse(tail, MissingTypeDefinition(fieldType) :: errors)

                    // We found it but it's a leaf type so we can't recurse into its children,
                    // instead just carry on with the parent types other selections (tail)
                    case Some(_: ScalarTypeDefinition | _: EnumTypeDefinition) =>
                      // 5.3.3 leaf field selection validation
                      if selectionSet.isEmpty then recurse(tail, errors)
                      else recurse(tail, IllegalSelection(fieldName, parentType) :: errors)

                    // We found an object type so we need to recurse
                    case Some(typeDef) =>
                      // 5.3.3 leaf field selection validation
                      if selectionSet.isEmpty then MissingSelection(fieldName, parentType) :: errors
                      else
                        val typeAndSelection = selectionSet.map(NamedType(typeDef.name) -> _)
                        recurse(typeAndSelection ::: tail, errors)
                  end match

            case InlineFragment(Some(onType), _, selectionSet) =>
              val typeErrs         = validateObjectLikeTypeDefExists(onType, schema)
              val typeAndSelection = selectionSet.map(onType -> _).toList

              // 5.5.2.3.1
              val spreadTypeErr = if onType == parentType then None else Some(IllegalType(onType))

              recurse(typeAndSelection ::: tail, typeErrs ::: accErrs)

            // Type name has been omitted so this inline fragment has the same type as enclosing
            // context (e.g. the current parentType)
            case InlineFragment(None, _, selectionSet) =>
              val typeErrs         = validateObjectLikeTypeDefExists(parentType, schema)
              val typeAndSelection = selectionSet.map(parentType -> _).toList
              recurse(typeAndSelection ::: tail, typeErrs ::: accErrs)

            // Fragment definitions have already been validated by this point so you only need to
            // check if the fragment definition exists, there's no need to step into the defintion
            case FragmentSpread(name, _) =>
              // 5.5.2.1 Fragment definition must exist
              doc.findFragDef(name) match
                case None => recurse(tail, MissingFragmentDefinition(name) :: accErrs)
                case _    => recurse(tail, accErrs)
    end recurse

    recurse(selectionSet.map(parentType -> _))
  end validateSelectionSet

  private def validateOperationDefinition(
      opDef: OperationDefinition,
      fragDefReqs: FragDefReqsTable,
      doc: ExecutableDocument,
      schema: TypeSystemDocument
  ): List[GqlError] =
    val opDefReqs = findOpDefReqs(opDef, fragDefReqs)

    // 5.8.1 unique variables
    val duplicateVarErrs = opDef.variableDefinitions
      .groupBy(_.name)
      .flatMap {
        case (name, vars) if vars.length > 1 => List(DuplicateVariable(name))
        case _                               => Nil
      }
      .toList

    // 5.8.2 variable type must be an input type
    val inputTypeErrs = opDef.variableDefinitions
      .filter(varDef => !schema.isInputType(varDef.tpe))
      .map(varDef => IllegalType(varDef.tpe))

    // 5.8.3 variable uses defined
    val varDefinedErrs = opDefReqs
      .flatMap(variable =>
        if opDef.variableDefinitions.exists(_.name == variable.name) then None
        else Some(MissingVariable(variable.name))
      )
      .toList

    // 5.8.4 all variables used
    val unusedVarErrs = opDef.variableDefinitions.flatMap(varDef =>
      if opDefReqs.exists(_.name == varDef.name) then None
      else Some(UnusedVariable(varDef.name))
    )

    val selectionSetErrs = schema.findOpTypeDef(opDef.operationType) match
      case None => MissingOperationTypeDefinition(opDef.operationType) :: Nil
      case Some(typeDef) =>
        validateSelectionSet(opDef.selectionSet.toList, NamedType(typeDef.name), doc, schema)

    duplicateVarErrs ::: inputTypeErrs ::: varDefinedErrs ::: unusedVarErrs ::: selectionSetErrs
  end validateOperationDefinition

  private def validateFragmentDefinition(
      fragDef: FragmentDefinition,
      doc: ExecutableDocument,
      schema: TypeSystemDocument
  ): List[GqlError] =
    val selectionErrs = validateSelectionSet(fragDef.selectionSet.toList, fragDef.on, doc, schema)
    val typeErrs      = validateObjectLikeTypeDefExists(fragDef.on, schema)
    selectionErrs ::: typeErrs

  private def validateOperationDefinitions(
      fragDefReqs: FragDefReqsTable,
      doc: ExecutableDocument,
      schema: TypeSystemDocument
  ): List[GqlError] =
    val opDefs = doc.definitions.collect { case o: OperationDefinition => o }

    // 5.2.1.1 unique operation names
    val uniquenessErrs = opDefs
      .groupBy(_.name)
      .filter { case name -> xs => name.isDefined && xs.length > 1 }
      .map { case name -> _ => DuplicateOperationDefinition(name.get) }
      .toList

    // 5.2.2.1 Lone anonymous operation
    val namedOps = opDefs.filter(_.name.isDefined)
    val anonOps  = opDefs.filter(_.name.isEmpty)
    val loneAnonErrs =
      if anonOps.length > 1 then MultipleAnonymousQueries :: Nil
      else if anonOps.length == 1 && !namedOps.isEmpty then AnonymousQueryNotAlone :: Nil
      else Nil

    val errs = opDefs.flatMap(validateOperationDefinition(_, fragDefReqs, doc, schema))

    uniquenessErrs ::: loneAnonErrs ::: errs
  end validateOperationDefinitions

  private def validateFragmentDefinitions(
      doc: ExecutableDocument,
      schema: TypeSystemDocument
  ): Either[List[GqlError], FragDefReqsTable] =
    val fragDefs = doc.definitions.collect { case o: FragmentDefinition => o }

    // 5.5.2.2 Fragment definitions must not contain cycles
    // topologically sorting the fragment dependency graph will find cycles and provide an order to
    // find fragment definition requirements
    topologicalSort(buildFragDefDepGraph(fragDefs))
      .flatMap(sortedGraph =>
        // 5.5.1.1 fragment definition unique name
        val uniquenessErrs = fragDefs
          .groupBy(_.name)
          .filter { case _ -> xs => xs.length > 1 }
          .map { case Some(name) -> _ => DuplicateFragmentDefinition(name) }
          .toList

        // 5.5.1.4 Fragment definitions must be used
        val fragDefNames = fragDefs.map(_.name.get)
        val fragSpreads  = doc.findFragSpreads().map(_.name)
        val unusedErrs = fragDefNames
          .filterNot(fragSpreads.contains)
          .map(UnusedFragment(_))

        // 5.5.2.1 Fragment definition must exist
        val missingFragDefErrs = sortedGraph
          .flatMap((name, deps) =>
            deps.flatMap(depName =>
              if sortedGraph.exists((fragDef, _) => fragDef.name.get == depName) then None
              else Some(MissingFragmentDefinition(depName))
            )
          )
          .toList

        val fragDefErrs = fragDefs.flatMap(validateFragmentDefinition(_, doc, schema))
        val errs        = uniquenessErrs ::: unusedErrs ::: missingFragDefErrs ::: fragDefErrs

        errs match
          case Nil =>
            if sortedGraph.isEmpty then Map.empty.asRight
            else
              // convert sorted graph to list of sorted frag defs
              val sortedFragDefs = sortedGraph.map((fragDef, _) => fragDef)

              val rootFragDefReqs =
                findFragDefReqs(sortedFragDefs.head, Map.empty, doc)

              sortedFragDefs
                .foldLeft(rootFragDefReqs)((accReqs, fragDef) =>
                  findFragDefReqs(fragDef, accReqs, doc)
                )
                .asRight
          case errs => errs.asLeft
      )
  end validateFragmentDefinitions

  def validate(
      doc: ExecutableDocument,
      schema: TypeSystemDocument
  ): Either[NonEmptyList[GqlError], ExecutableDocument] =
    // 5.1.1
    // This is implied by the fact that the validate function expects doc to be an ExecutableDocument

    val fragmentErrs = validateFragmentDefinitions(doc, schema)

    // if any fragment errs exist then bail and return them
    fragmentErrs match
      case Left(errs) => NonEmptyList.fromListUnsafe(errs).asLeft
      case Right(reqs) =>
        val operationErrs    = validateOperationDefinitions(reqs, doc, schema)
        val subscriptionErrs = validateSubscriptionsHaveSingleRoot(doc)
        val errs             = operationErrs ::: subscriptionErrs
        errs match
          case Nil  => doc.asRight
          case errs => NonEmptyList.fromListUnsafe(errs).asLeft
  end validate

// 5.3.2
// TODO: Implement this validator.

// 5.5.2.3.1
// TODO: Implement this validator.

// 5.5.2.3.2
// TODO: Implement this validator.

// 5.5.2.3.3
// TODO: Implement this validator.

// 5.5.2.3.4
// TODO: Implement this validator.

// 5.6.1
// TODO: Implement this validator.

// 5.7.1
// TODO: Implement this validator.

// 5.7.2
// TODO: Implement this validator.

// 5.7.3
// TODO: Implement this validator.

// 5.8.5
// TODO: Implement this validator.

end DocumentValidator

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

object DocumentValidator:
  /** A table mapping definition names to the set of variables required to execute that definition.
    */
  type FragDefReqsTable = Map[Name, Set[Variable]]

  /** A mapping from fragment defintion to its dependencies */
  // type FragDefDepsGraph =
  //   LinkedHashMap[FragmentDefinition, Set[Name]] // maintains insertion order 👍

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
                  recurse(tail, accReqs + (fragDef.name -> depReqs(fragDef.name)))
          end match
      end match
    end recurse

    val selectionSet           = fragDef.selectionSet.toList.map(fragDef.name -> _)
    val reqs: FragDefReqsTable = Map(fragDef.name -> Set.empty)
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

  ///////////////////////////////////////////////////////////////////////////////////////////////////
  // Validators

  /**   - 5.2.3.1 Checks to see if the used fragment has a single root
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

  /**   - 5.5.1.2 Fragment types should exist.
    *   - 5.5.1.3 Fragments must reference either Union, Interface or Object types.
    */
  // TODO: Spec contains error - formal spec explicitly mentions named spreads which implies
  //       inlined fragments are not covered by this validation rule, but they clearly are!
  def validateObjectLikeTypeDefExists(
      namedType: NamedType
  )(using schema: TypeSystemDocument): Validated[NamedType] =
    schema.findTypeDef[TypeDefinition](namedType.name) match
      case Some(_: ObjectTypeDefinition | _: InterfaceTypeDefinition | _: UnionTypeDefinition) =>
        namedType.validNec
      case Some(typeDef) => InvalidNamedType(typeDef.name).invalidNec
      case None          => MissingDefinition(namedType.name).invalidNec
  end validateObjectLikeTypeDefExists

  /**   - 5.6.2 input object field exists
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
                    .map(_.name)
                    .map(name =>
                      value.fields.find(_.name == name) match
                        case None =>
                          MissingField2(name, NamedType(inObjTypeDef.name)).invalidNec[Unit]
                        case Some(_) => ().validNec[GqlError]
                    )
                    .reduceOption(_ combine _)
                    .getOrElse(().validNec)

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
      parentType: NamedType
  )(using schema: TypeSystemDocument): Validated[List[Argument]] =
    println(field)
    println(fieldDef)
    println("--------++++++++--------")
    // TODO: Choose better name for this
    val validatedArgs =
      field.arguments.map {
        case Argument(name, objVal: ObjectValue) =>
          // 5.4.1 args exist
          // TODO: Include directives in this
          fieldDef.arguments.find(_.name == name) match
            case None =>
              MissingDefinition(
                name,
                Some(s"in field ${field.name} defined in type ${parentType}")
              ).invalidNec
            case Some(inValDef) =>
              validateInputObjectValue(objVal, inValDef).map(_ => ())

        case Argument(name, _) =>
          // 5.4.1 args exist
          // TODO: Include directives in this
          fieldDef.arguments.find(_.name == name) match
            case None =>
              MissingDefinition(
                name,
                Some(s"in field ${field.name} defined in type ${parentType}")
              ).invalidNec
            case _ => ().validNec
      }.combineAll

    // 5.4.2 args are unique
    // TODO: Include directives in this
    val validatedArgNames = validateUniqueName(field.arguments).map(_ => ())

    // 5.4.2.1 required args
    // TODO: Include directives in this
    val requiredArgs =
      fieldDef.arguments
        .filter {
          case InputValueDefinition(_, _: NonNullType, None, _) => true
          case _                                                => false
        }
        .map(_.name)
    val validatedRequiredArgs =
      requiredArgs
        .map(argName =>
          field.arguments.find(_.name == argName) match
            case None    => MissingArgument2(argName, Some(s"field.name, parentType")).invalidNec
            case Some(_) => ().validNec
        )
        .reduceOption(_ combine _)
        .getOrElse(().validNec)

    (
      validatedArgs,
      validatedArgNames,
      validatedRequiredArgs
    ).mapN((v1, v2, v3) => field.arguments)
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
            case field @ Field(_, fieldName, arguments, _, selectionSet) =>
              // 5.3.1 field selections must exist on object, interface and union types
              schema.findFieldDef(fieldName, parentType) match
                case None =>
                  val validatedField = MissingField2(fieldName, parentType).invalidNec
                  validatedField combine acc
                case Some(fieldDef) =>
                  val validatedArgs = validateArguments(field, fieldDef, parentType).map(_ => ())
                  // val errors  = argErrs ::: accErrs

                  val fieldType: NamedType = NamedType(fieldDef.`type`.name)
                  schema.findTypeDef[TypeDefinition](fieldType.name) match
                    case None =>
                      val validatedDef = MissingTypeDefinition(fieldType).invalidNec
                      recurse(tail, validatedDef combine acc)

                    // We found it but it's a leaf type so we can't recurse into its children,
                    // instead just carry on with the parent types other selections (tail)
                    case Some(_: ScalarTypeDefinition | _: EnumTypeDefinition) =>
                      // 5.3.3 leaf field selection validation
                      if selectionSet.isEmpty then recurse(tail, validatedArgs combine acc)
                      else
                        val validatedSelection = InvalidSelection(fieldName, parentType).invalidNec
                        recurse(
                          tail,
                          validatedArgs combine validatedSelection combine acc
                        )

                    // We found an object type so we need to recurse
                    case Some(typeDef) =>
                      // 5.3.3 leaf field selection validation
                      if selectionSet.isEmpty then
                        val validatedSelection = MissingSelection2(fieldName, parentType).invalidNec
                        validatedArgs combine validatedSelection combine acc
                      else
                        val typeAndSelection = selectionSet.map(NamedType(typeDef.name) -> _)
                        recurse(typeAndSelection ::: tail, validatedArgs combine acc)
                  end match

            case InlineFragment(Some(onType), _, selectionSet) =>
              val validatedTypeDef = validateObjectLikeTypeDefExists(onType).map(_ => ())
              val typeAndSelection = selectionSet.map(onType -> _).toList

              // 5.5.2.3.1
              val validatedSpread =
                if onType == parentType then ().validNec[GqlError]
                else InvalidType(onType).invalidNec

              recurse(
                typeAndSelection ::: tail,
                validatedTypeDef combine validatedSpread combine acc
              )

            // Type name has been omitted so this inline fragment has the same type as enclosing
            // context (e.g. the current parentType)
            case InlineFragment(None, _, selectionSet) =>
              val validatedTypeDef = validateObjectLikeTypeDefExists(parentType).map(_ => ())
              val typeAndSelection = selectionSet.map(parentType -> _).toList
              recurse(typeAndSelection ::: tail, validatedTypeDef combine acc)

            // Fragment definitions have already been validated by this point so you only need to
            // check if the fragment definition exists, there's no need to step into the defintion
            case FragmentSpread(name, _) =>
              // 5.5.2.1 Fragment definition must exist
              doc.findFragDef(name) match
                case None =>
                  val validatedFragDef = MissingDefinition(name).invalidNec
                  recurse(tail, validatedFragDef combine acc)
                case _ => recurse(tail, acc)
    end recurse

    recurse(selectionSet.map(parentType -> _), ().validNec).map(_ => selectionSet)
  end validateSelectionSet

  private def validateOperationDefinition(
      opDef: OperationDefinition,
      fragDefReqs: FragDefReqsTable
  )(using
      schema: TypeSystemDocument,
      doc: ExecutableDocument
  ): Validated[OperationDefinition] =
    val opDefReqs = findOpDefReqs(opDef, fragDefReqs)

    // 5.8.1 unique variables
    val validatedVariableNames = validateUniqueName(opDef.variableDefinitions)

    println(opDef.variableDefinitions)

    // 5.8.2 variable type must be an input type
    val validatedVariableTypes =
      opDef.variableDefinitions
        .map(varDef =>
          if schema.isInputType(varDef.`type`) then ().validNec
          else InvalidType(varDef.`type`).invalidNec
        )
        .reduceOption(_ combine _)
        .getOrElse(().validNec)

    // 5.8.3 variable uses defined
    val validatedVariablesDefined =
      opDefReqs
        .map(variable =>
          if opDef.variableDefinitions.exists(_.name == variable.name) then ().validNec
          else MissingVariable2(variable.name).invalidNec
        )
        .reduceOption(_ combine _)
        .getOrElse(().validNec)

    // 5.8.4 all variables used
    val validatedVariablesUsed = opDef.variableDefinitions
      .map(varDef =>
        if opDefReqs.exists(_.name == varDef.name) then ().validNec
        else UnusedDefinition(varDef.name).invalidNec
      )
      .reduceOption(_ combine _)
      .getOrElse(().validNec)

    val validatedSelectionSets = schema.findOpTypeDef(opDef.operationType) match
      case None =>
        MissingDefinition(Name("")).invalidNec // TODO: Need to handle non named type defs somehow
      case Some(typeDef) => validateSelectionSet(opDef.selectionSet.toList, NamedType(typeDef.name))

    (
      validatedVariableNames,
      validatedVariableTypes,
      validatedVariablesDefined,
      validatedVariablesUsed,
      validatedSelectionSets
    ).mapN((_, _, _, _, _) => opDef)
  end validateOperationDefinition

  private def validateFragmentDefinition(
      fragDef: FragmentDefinition
  )(using doc: ExecutableDocument, schema: TypeSystemDocument): Validated[FragmentDefinition] =
    (
      validateSelectionSet(fragDef.selectionSet.toList, fragDef.on),
      validateObjectLikeTypeDefExists(fragDef.on)
    ).mapN((_, _) => fragDef)
  end validateFragmentDefinition

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
      opDefs
        .map(validateOperationDefinition(_, fragDefReqs).map(List(_)))
        .reduceOption(_ combine _)
        .getOrElse(Nil.validNec)

    (
      // 5.2.1.1 unique operation names
      validateUniqueName(opDefs),

      // 5.2.2.1 Lone anonymous operation
      validateAnonymousOperationDefinition(namedOps, anonOps),

      // validate each operation definition
      validatedOpDefs
    ).mapN((_, _, validatedOpDefs) => opDefs)

    // // 5.2.1.1 unique operation names
    // val uniquenessErrs = opDefs
    //   .groupBy(_.name)
    //   .filter { case name -> xs => name.isDefined && xs.length > 1 }
    //   .map { case name -> _ => DuplicateOperationDefinition(name.get) }
    //   .toList

    // // 5.2.2.1 Lone anonymous operation
    // val namedOps = opDefs.filter(_.name.isDefined)
    // val anonOps  = opDefs.filter(_.name.isEmpty)
    // val loneAnonErrs =
    //   if anonOps.length > 1 then MultipleAnonymousQueries :: Nil
    //   else if anonOps.length == 1 && !namedOps.isEmpty then AnonymousQueryNotAlone :: Nil
    //   else Nil

    // val errs = opDefs.flatMap(validateOperationDefinition(_, fragDefReqs, doc, schema))

    // uniquenessErrs ::: loneAnonErrs ::: errs
  end validateOperationDefinitions

  private def validateFragDefsExist[T <: HasName](
      sortedGraph: DependencyGraph[T]
  ): Validated[List[Name]] =
    sortedGraph.toList
      .traverse { case (name, deps) =>
        deps.toList.traverse(depName =>
          if sortedGraph.exists((fragDef, _) => fragDef.name == depName) then depName.validNec
          else MissingDefinition(depName).invalidNec
        )
      }
      .map(_.flatten)

  private def validateFragmentDefinitions(using
      schema: TypeSystemDocument,
      doc: ExecutableDocument
  ): Validated[FragDefReqsTable] =
    val fragDefs = doc.findExecDef[FragmentDefinition]

    // 5.5.2.2 Fragment definitions must not contain cycles
    // topologically sorting the fragment dependency graph will find cycles and provide an order to
    // find fragment definition requirements
    topologicalSort(buildFragDefDepGraph(fragDefs))
      .andThen(sortedGraph =>
        (
          // 5.5.1.1 fragment definition unique name
          validateUniqueName(fragDefs),

          // 5.5.1.4 Fragment definitions must be used
          validateIsUsed(fragDefs, doc.findFragSpreads()),

          // 5.5.2.1 Fragment definition must exist
          validateFragDefsExist(sortedGraph)
        ).mapN((_, _, _) =>
          if sortedGraph.isEmpty then Map.empty
          else
            val sortedFragDefs = sortedGraph.map((fragDef, _) => fragDef)
            findFragDefReqs(sortedFragDefs.head, Map.empty, doc)
        )
      )

    // 5.5.2.2 Fragment definitions must not contain cycles
    // topologically sorting the fragment dependency graph will find cycles and provide an order to
    // find fragment definition requirements
    // topologicalSort(buildFragDefDepGraph(fragDefs))
    //   .flatMap(sortedGraph =>
    //     // 5.5.1.1 fragment definition unique name
    //     val uniquenessErrs = fragDefs
    //       .groupBy(_.name)
    //       .filter { case _ -> xs => xs.length > 1 }
    //       .map { case (name, _) => DuplicateFragmentDefinition(name) }
    //       .toList

    //     // 5.5.1.4 Fragment definitions must be used
    //     val fragDefNames = fragDefs.map(_.name)
    //     val fragSpreads  = doc.findFragSpreads().map(_.name)
    //     val unusedErrs = fragDefNames
    //       .filterNot(fragSpreads.contains)
    //       .map(UnusedFragment(_))

    //     // 5.5.2.1 Fragment definition must exist
    //     val missingFragDefErrs = sortedGraph
    //       .flatMap((name, deps) =>
    //         deps.flatMap(depName =>
    //           if sortedGraph.exists((fragDef, _) => fragDef.name == depName) then None
    //           else Some(MissingFragmentDefinition(depName))
    //         )
    //       )
    //       .toList

    //     val fragDefErrs = fragDefs.flatMap(validateFragmentDefinition(_, doc, schema))
    //     val errs        = uniquenessErrs ::: unusedErrs ::: missingFragDefErrs ::: fragDefErrs

    //     errs match
    //       case Nil =>
    //         if sortedGraph.isEmpty then Map.empty.asRight
    //         else
    //           // convert sorted graph to list of sorted frag defs
    //           val sortedFragDefs = sortedGraph.map((fragDef, _) => fragDef)

    //           val rootFragDefReqs =
    //             findFragDefReqs(sortedFragDefs.head, Map.empty, doc)

    //           sortedFragDefs
    //             .foldLeft(rootFragDefReqs)((accReqs, fragDef) =>
    //               findFragDefReqs(fragDef, accReqs, doc)
    //             )
    //             .asRight
    //       case errs => errs.asLeft
    //   )
  end validateFragmentDefinitions

  def validate(doc: ExecutableDocument)(using TypeSystemDocument): Validated[ExecutableDocument] =
    given ExecutableDocument = doc

    // 5.1.1
    // This is implied by the fact that the validate function expects doc to be an ExecutableDocument

    validateFragmentDefinitions.andThen(reqs =>
      (
        validateOperationDefinitions(reqs),
        validateSubscriptionsHaveSingleRoot
      ).mapN((_, _) => doc)
    )

    // if any fragment errs exist then bail and return them
    // fragmentErrs match
    //   case Left(errs) => NonEmptyList.fromListUnsafe(errs).asLeft
    //   case Right(reqs) =>
    //     val operationErrs    = validateOperationDefinitions(reqs, doc, schema)
    //     val subscriptionErrs = validateSubscriptionsHaveSingleRoot(doc)
    //     val errs             = operationErrs ::: subscriptionErrs
    //     errs match
    //       case Nil  => doc.asRight
    //       case errs => NonEmptyList.fromListUnsafe(errs).asLeft
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

// Copyright (c) 2021 by Oliver Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s

import cats.data.NonEmptyList
import cats.implicits.*
import scala.collection.immutable.HashMap
import scala.annotation.tailrec

import GqlError.*
import OperationType.*
import Selection.*
import Type.*
import Value.*

/** A table mapping definition names to the set of variables required to execute that definition. */
// TODO: Remove this when the reverse topological validation of frag defs is complete
type VarReqs = Map[Name, Set[Variable]]

///////////////////////////////////////////////////////////////////////////////////////////////////
// Helper Functions
//
// QUESTION: The results of these functions could perhaps be calculated once and stored in a single
//           place like a Symbol Table

private def findFragDef(fragName: Name, doc: ExecutableDocument): Option[FragmentDefinition] =
  doc.collect { case f: FragmentDefinition => f }.find(_.name.get == fragName)

private def findObjTypeDef(
    namedType: NamedType,
    schema: TypeSystemDocument
): Option[ObjectTypeDefinition] =
  schema.collect { case o: ObjectTypeDefinition => o }.find(_.name == namedType.name)

private def findInputObjTypeDef(
    name: Name,
    schema: TypeSystemDocument
): Option[InputObjectTypeDefinition] =
  schema.collect { case o: InputObjectTypeDefinition => o }.find(_.name == name)

private def findTypeDef(namedType: NamedType, schema: TypeSystemDocument): Option[TypeDefinition] =
  namedType match
    case NamedType(Name("Int"))     => Some(ScalarTypeDefinition(Name("Int"), Nil))
    case NamedType(Name("Float"))   => Some(ScalarTypeDefinition(Name("Float"), Nil))
    case NamedType(Name("String"))  => Some(ScalarTypeDefinition(Name("String"), Nil))
    case NamedType(Name("Boolean")) => Some(ScalarTypeDefinition(Name("Boolean"), Nil))
    case NamedType(Name("ID"))      => Some(ScalarTypeDefinition(Name("ID"), Nil))
    case _ => schema.collect { case t: TypeDefinition => t }.find(_.name == namedType.name)

/** Checks whether the given field exists within the given type.
  *
  * @param fieldName
  *   The field we're looking for.
  * @param namedType
  *   The name of the type to search in.
  * @param schema
  *   The graphql schema.
  * @return
  *   Some MissingField error if the field cannot be found, None if it can.
  */
private def findFieldDef(
    fieldName: Name,
    namedType: NamedType,
    schema: TypeSystemDocument
): Option[FieldDefinition] =
  @tailrec
  def recurse(namedTypes: List[NamedType]): Option[FieldDefinition] =
    namedTypes match
      case Nil => None
      case namedType :: tail =>
        val typeDef = findTypeDef(namedType, schema)

        typeDef match
          case Some(ObjectTypeDefinition(_, interfaces, _, fields)) =>
            val fieldDef = fields.find(_.name == fieldName)
            if fieldDef.isDefined then fieldDef
            else recurse(interfaces ::: tail)

          case Some(InterfaceTypeDefinition(_, interfaces, _, fields)) =>
            val fieldDef = fields.find(_.name == fieldName)
            if fieldDef.isDefined then fieldDef
            else recurse(interfaces ::: tail)

          case Some(UnionTypeDefinition(_, _, members)) =>
            recurse(members ::: tail)

          // The type that we're checking exists but isn't a type with fields
          // therefore the field can't exist so we just return false
          case _ => None
  end recurse

  recurse(List(namedType))
end findFieldDef

private def findOperationTypeDef(
    opType: OperationType,
    schema: TypeSystemDocument
): Option[ObjectTypeDefinition] =
  val schemaDef = schema.collect { case s: SchemaDefinition => s }.headOption
  schemaDef match
    case Some(SchemaDefinition(_, roots)) =>
      roots
        .find(_.operationType == opType)
        .map(_.namedType)
        .flatMap(findObjTypeDef(_, schema))
    case None =>
      opType match
        case Query        => findObjTypeDef(NamedType(Name("Query")), schema)
        case Mutation     => findObjTypeDef(NamedType(Name("Mutation")), schema)
        case Subscription => findObjTypeDef(NamedType(Name("Subscription")), schema)
end findOperationTypeDef

/** Finds unique uses of fragment spreads.
  *
  * @return
  *   A list of fragment spreads which are used in the given document. No duplicates will exist in
  *   the list.
  */
private def findFragmentSpreads(doc: ExecutableDocument): List[FragmentSpread] =
  @tailrec
  def recurse(accSelectionSet: List[Selection], acc: List[FragmentSpread]): List[FragmentSpread] =
    accSelectionSet match
      case Nil => acc
      case head :: tail =>
        head match
          case Field(_, _, _, _, selectionSet)    => recurse(selectionSet ::: tail, acc)
          case InlineFragment(_, _, selectionSet) => recurse(selectionSet.toList ::: tail, acc)
          case spread: FragmentSpread =>
            val acc2 = if acc.contains(spread) then acc else spread :: acc
            recurse(tail, acc2)
  end recurse

  doc
    .collect { case o: OperationDefinition => o }
    .map(_.selectionSet.toList)
    .flatMap(recurse(_, Nil))
end findFragmentSpreads

def isLeafType(`type`: Type): Boolean = `type`.name match
  case Name("Int") | Name("Float") | Name("String") | Name("Boolean") | Name("ID") => true
  case Name(_)                                                                     => false

def isInputType(`type`: Type, schema: TypeSystemDocument): Boolean =
  isLeafType(`type`) || findInputObjTypeDef(`type`.name, schema).isDefined

/** Builds a table of variables required to execute the given fragment definition
  */
// TODO: Do a topological sort on the fragment definitions and call this function in the order of
//       that sort.
//       Add the current requirement map as a param to this function.
//       Don't recurse into child fragment definitions because their requirements have already been
//       found.
//       Return a single Set[Varialbe] from this function.
def findFragmentDefinitionRequirements(
    fragDef: FragmentDefinition,
    doc: ExecutableDocument
): VarReqs =
  @tailrec
  def recurse(
      accSelectionSets: List[(Name, Selection)],
      accReqs: VarReqs = Map.empty
  ): VarReqs =
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
            findFragDef(frag.name, doc) match
              case None =>
                // TODO: Is this even possible or are all frag defs shown to exist by this point?
                recurse(tail, accReqs)

              case Some(fragDef) =>
                val accSelectionSet = fragDef.selectionSet.toList.map(fragDef.name.get -> _)
                val reqs            = fragDef.name.get -> Set.empty[Variable]
                recurse(accSelectionSet ::: tail, accReqs + reqs)
        end match
    end match
  end recurse

  val selectionSet  = fragDef.selectionSet.toList.map(fragDef.name.get -> _)
  val reqs: VarReqs = Map(fragDef.name.get -> Set.empty)
  recurse(selectionSet, reqs)
end findFragmentDefinitionRequirements

def findOperationDefinitionRequirements(
    opDef: OperationDefinition,
    fragDefReqs: Map[Name, Set[Variable]]
): Set[Variable] =
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
        if fragDefReqs.contains(frag.name) then recurse(tail, fragDefReqs(frag.name) union accReqs)
        else recurse(tail, accReqs)

    end match
  end recurse

  recurse(opDef.selectionSet.toList)
end findOperationDefinitionRequirements

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

  val ops   = doc.collect { case x: OperationDefinition => x }
  val frags = doc.collect { case x: FragmentDefinition => x }
  val subs  = ops.filter(_.operationType == Subscription)

  // try to find a sub with multiple roots
  val multipleRoots = subs.find {
    case OperationDefinition(_, _, _, _, NonEmptyList(f: (InlineFragment | FragmentSpread), Nil)) =>
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
  findTypeDef(namedType, schema) match
    case Some(_: ObjectTypeDefinition | _: InterfaceTypeDefinition | _: UnionTypeDefinition) => Nil
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
            findInputObjTypeDef(inValDef.tpe.name, schema) match
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
          case None    => recurse(tail, MissingField(name, NamedType(inObjTypeDef.name)) :: accErrs)
          case Some(_) => recurse(tail, accErrs)
  end recurse

  // 5.6.3 input object field duplicates
  val duplicateErrs = inObjVal.fields
    .groupBy(_.name)
    .filter(_._2.length > 1)
    .map { case (name, _) => DuplicateField(name) }
    .toList

  findInputObjTypeDef(inValDef.tpe.name, schema) match
    case None               => MissingInputObjectTypeDefinition(inValDef.tpe.name) :: duplicateErrs
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
// TODO: This function is WAY too complicated! It needs to be broken down and documented better.
//       I really don't like the added complication of checking for cycles within fragment
//       definitions, it's why fragDef is passed to it and why accFragDefs is passed to the
//       recurse function. It feels as though cycle validation should be factored out into a
//       separate function but it's not easy to do that without repeating code but maybe that's
//       the lesser of two evils?

// TODO: Add 2 bits to fragmentdef validation function, 1 which checks for cycles, 1 which builds a table
//       of used variables (requirements of the fragment).
//       Remove the fragment cycle check from validateSelectionSet.
//       Add specialised recursive check to validateOperationDefinition that checks variables are defined
//       and all variables are used. This requires that the fragment requirements table is passed to
//       validateOperationDefinition.
//       Add step to main validate function which stops any further validation taking place if any cycles
//       are found in the validateFragmentDefinitions step.
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
            findFieldDef(fieldName, parentType, schema) match
              case None => MissingField(fieldName, parentType) :: accErrs
              case Some(fieldDef) =>
                val argErrs = validateArguments(field, fieldDef, parentType, schema)
                val errors  = argErrs ::: accErrs

                val fieldType: NamedType = NamedType(fieldDef.tpe.name)
                findTypeDef(fieldType, schema) match
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
            findFragDef(name, doc) match
              case None => recurse(tail, MissingFragmentDefinition(name) :: accErrs)
              case _    => recurse(tail, accErrs)
  end recurse

  recurse(selectionSet.map(parentType -> _))
end validateSelectionSet

private def validateOperationDefinition(
    opDef: OperationDefinition,
    fragDefReqs: Map[Name, Set[Variable]],
    doc: ExecutableDocument,
    schema: TypeSystemDocument
): List[GqlError] =
  val opDefReqs = findOperationDefinitionRequirements(opDef, fragDefReqs)

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
    .filter(varDef => !isInputType(varDef.tpe, schema))
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

  val selectionSetErrs = findOperationTypeDef(opDef.operationType, schema) match
    case None => MissingOperationTypeDefinition(opDef.operationType) :: Nil
    case Some(typeDef) =>
      validateSelectionSet(opDef.selectionSet.toList, NamedType(typeDef.name), doc, schema)

  duplicateVarErrs ::: inputTypeErrs ::: varDefinedErrs ::: unusedVarErrs ::: selectionSetErrs
end validateOperationDefinition

private def validateFragmentDefinition(
    fragDef: FragmentDefinition,
    doc: ExecutableDocument,
    schema: TypeSystemDocument
): Either[List[GqlError], VarReqs] =
  // QUESTION: Maybe this function should be factored out into a separate function
  @tailrec
  def recurse(
      accSelectionSet: List[Selection],
      accFragDefs: NonEmptyList[Name],
      accErrs: List[GqlError] = Nil
  ): List[GqlError] =
    accSelectionSet match
      case Nil => accErrs
      case selection :: tail =>
        selection match
          case field: Field => recurse(field.selectionSet ::: tail, accFragDefs, accErrs)

          case frag: InlineFragment =>
            recurse(frag.selectionSet.toList ::: tail, accFragDefs, accErrs)

          case frag: FragmentSpread =>
            // 5.5.2.1 Fragment definition must exist
            val fragDef = findFragDef(frag.name, doc)

            // 5.5.2.2 Fragment definitions must not contain cycles
            val containsCycles = accFragDefs.exists(_ == frag.name)

            if containsCycles then FragmentContainsCycles(frag.name) :: accErrs
            else
              fragDef match
                case None =>
                  recurse(tail, accFragDefs, MissingFragmentDefinition(frag.name) :: accErrs)
                case Some(fragDef) =>
                  recurse(fragDef.selectionSet.toList ::: tail, frag.name :: accFragDefs, accErrs)
        end match
    end match
  end recurse

  val fragDefErrs   = recurse(fragDef.selectionSet.toList, NonEmptyList.one(fragDef.name.get))
  val selectionErrs = validateSelectionSet(fragDef.selectionSet.toList, fragDef.on, doc, schema)
  val typeErrs      = validateObjectLikeTypeDefExists(fragDef.on, schema)

  val errs = fragDefErrs ::: selectionErrs ::: typeErrs

  errs match
    case Nil  => findFragmentDefinitionRequirements(fragDef, doc).asRight
    case errs => errs.asLeft
end validateFragmentDefinition

private def validateOperationDefinitions(
    fragDefReqs: VarReqs,
    doc: ExecutableDocument,
    schema: TypeSystemDocument
): List[GqlError] =
  val opDefs = doc.collect { case o: OperationDefinition => o }

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
): Either[List[GqlError], VarReqs] =
  val fragDefs = doc.collect { case o: FragmentDefinition => o }

  // 5.5.1.1 fragment definition unique name
  val uniquenessErrs = fragDefs
    .groupBy(_.name)
    .filter { case _ -> xs => xs.length > 1 }
    .map { case Some(name) -> _ => DuplicateFragmentDefinition(name) }
    .toList

  // 5.5.1.4 Fragment definitions must be used
  val fragDefNames = fragDefs.map(_.name.get)
  val fragSpreads  = findFragmentSpreads(doc).map(_.name)
  val unusedErrs = fragDefNames
    .filterNot(fragSpreads.contains)
    .map(UnusedFragment(_))

  val errsSoFar = uniquenessErrs ::: unusedErrs

  errsSoFar match
    case Nil =>
      fragDefs
        .traverse(validateFragmentDefinition(_, doc, schema))
        .leftMap(errs => errsSoFar ::: errs) // collect and return errors if left
        .map {
          case Nil  => Map.empty
          case reqs => reqs.reduce(_ combine _)
        } // combine and return reqs if right

    case errs => errs.asLeft
end validateFragmentDefinitions

def validate(
    doc: ExecutableDocument,
    schema: TypeSystemDocument
): Either[NonEmptyList[GqlError], ExecutableDocument] =
  // TODO: Topologically sort fragment defs and validate them in reverse order
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

// 5.1.1
// TODO: Implement this validator.

// 5.3.2 (10-2021)
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

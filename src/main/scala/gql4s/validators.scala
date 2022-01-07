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

///////////////////////////////////////////////////////////////////////////////////////////////////
// Helper Functions
//
// QUESTION: The results of these functions could perhaps be calculated once and stored in a single
//           place like a Symbol Table

private def findFragDef(fragName: Name, doc: ExecutableDocument): Option[FragmentDefinition] =
  doc.collect { case f: FragmentDefinition => f }.find(_.name == fragName)

private def findObjTypeDef(
    namedType: NamedType,
    schema: TypeSystemDocument
): Option[ObjectTypeDefinition] =
  schema.collect { case o: ObjectTypeDefinition => o }.find(_.name == namedType.name)

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

/** Performs various validation steps on the selection sets. Bear in mind that this function will
  * not validate fragment definitions but will validate inline fragment definitions, you must call
  * validateFragmentDefinition as well as this function to fully validate an executable document.
  *
  * @param selectionSet
  *   The selectionSet to recursively validate.
  * @param namedType
  *   The name of the type that the selectionSet is within.
  * @param schema
  *   The graphql schema.
  * @return
  *   Return a list of errors or Nil if there weren't any.
  */
private def validateSelectionSet(
    selectionSet: List[Selection],
    namedType: NamedType,
    doc: ExecutableDocument,
    schema: TypeSystemDocument
): List[GqlError] =
  @tailrec
  def recurse(
      accSelectionSet: List[(NamedType, Selection)],
      acc: List[GqlError] = Nil
  ): List[GqlError] =
    accSelectionSet match
      case Nil => acc
      case (namedType, selection) :: tail =>
        selection match
          case Field(_, fieldName, arguments, _, fieldSelectionSet) =>
            // 5.3.1
            findFieldDef(fieldName, namedType, schema) match
              case None           => MissingField(fieldName, Some(namedType)) :: acc
              case Some(fieldDef) =>
                // 5.4.1 args exist
                // TODO: Include directives in this
                val argExistenceErrors =
                  arguments
                    .flatMap { arg =>
                      fieldDef.arguments.find(_.name == arg.name) match
                        case None =>
                          MissingArgumentDefinition(arg.name, fieldName, namedType) :: Nil
                        case _ => Nil
                    }

                // 5.4.2 args are unique
                // TODO: Include directives in this
                val argUniquenessErrors = arguments
                  .groupBy(_.name)
                  .filter { case (argName, args) => args.size > 1 }
                  .map { case (argName, _) => DuplicateArgument(argName, fieldName, namedType) }
                  .toList

                // 5.4.2.1 required args
                // TODO: Include directives in this
                val requiredArgs = fieldDef.arguments
                  .filter {
                    case InputValueDefinition(_, _: NonNullType, None, _) => true
                    case _                                                => false
                  }
                  .map(_.name)
                val requiredArgErrors = requiredArgs
                  .filter(argName => arguments.find(_.name == argName).isEmpty)
                  .map(argName => MissingArgument(argName, fieldName, namedType))

                val accErrors =
                  argExistenceErrors ::: argUniquenessErrors ::: requiredArgErrors ::: acc

                val fieldNamedType: NamedType = NamedType(fieldDef.tpe.name)

                findTypeDef(fieldNamedType, schema) match
                  case None => MissingTypeDefinition(fieldNamedType) :: accErrors

                  // We found it but it's a leaf type so we can't recurse
                  case Some(_: ScalarTypeDefinition | _: EnumTypeDefinition) =>
                    // 5.3.3
                    if fieldSelectionSet.isEmpty then accErrors
                    else IllegalSelection(fieldName, namedType) :: accErrors

                  // We found an object type so we need to recurse
                  case Some(typeDef) =>
                    // 5.3.3
                    if fieldSelectionSet.isEmpty then
                      MissingSelection(fieldName, namedType) :: accErrors
                    else
                      val typeAndSelection = fieldSelectionSet.map(NamedType(typeDef.name) -> _)
                      recurse(typeAndSelection ::: tail, accErrors)

          case InlineFragment(Some(namedType), _, fragSelectionSet) =>
            val typeErrs = validateObjectLikeTypeDefExists(namedType, schema)

            val typeAndSelection = fragSelectionSet.map(namedType -> _).toList
            recurse(typeAndSelection ::: tail, typeErrs ::: acc)

          // Type name has been omitted so this inline fragment has the same type as enclosing
          // context (e.g. the current namedType)
          case InlineFragment(None, _, fragSelectionSet) =>
            val typeErrs         = validateObjectLikeTypeDefExists(namedType, schema)
            val typeAndSelection = fragSelectionSet.map(namedType -> _).toList
            recurse(typeAndSelection ::: tail, typeErrs ::: acc)

          // We have already validated the selection sets for fragment definitions by this point
          // so there's no need to recurse into the selection set for this fragment spreads.
          case FragmentSpread(name, _) =>
            // 5.5.2.1 Fragment definition must exist
            val fragDefExists =
              doc.collect { case o: FragmentDefinition => o }.exists(_.name.get == name)

            if fragDefExists then acc
            else MissingFragmentDefinition(name) :: acc
  end recurse

  recurse(selectionSet.map(namedType -> _))
end validateSelectionSet

private def validateOperationDefinition(
    opDef: OperationDefinition,
    doc: ExecutableDocument,
    schema: TypeSystemDocument
): List[GqlError] =
  findOperationTypeDef(opDef.operationType, schema) match
    case None => MissingOperationTypeDefinition(opDef.operationType) :: Nil
    case Some(typeDef) =>
      validateSelectionSet(opDef.selectionSet.toList, NamedType(typeDef.name), doc, schema)

private def validateFragmentDefinition(
    fragDef: FragmentDefinition,
    doc: ExecutableDocument,
    schema: TypeSystemDocument
): List[GqlError] =
  val selectionErrs = validateSelectionSet(fragDef.selectionSet.toList, fragDef.on, doc, schema)
  val typeErrs      = validateObjectLikeTypeDefExists(fragDef.on, schema)

  selectionErrs ::: typeErrs
end validateFragmentDefinition

private def validateOperationDefinitions(
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

  val errs = opDefs.flatMap(validateOperationDefinition(_, doc, schema))

  uniquenessErrs ::: loneAnonErrs ::: errs
end validateOperationDefinitions

private def validateFragmentDefinitions(
    doc: ExecutableDocument,
    schema: TypeSystemDocument
): List[GqlError] =
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

  val errs = fragDefs.flatMap(validateFragmentDefinition(_, doc, schema))

  uniquenessErrs ::: unusedErrs ::: errs
end validateFragmentDefinitions

def validate(
    doc: ExecutableDocument,
    schema: TypeSystemDocument
): Either[NonEmptyList[GqlError], ExecutableDocument] =
  val fragmentErrs     = validateFragmentDefinitions(doc, schema)
  val operationErrs    = validateOperationDefinitions(doc, schema)
  val subscriptionErrs = validateSubscriptionsHaveSingleRoot(doc)

  fragmentErrs ::: operationErrs ::: subscriptionErrs match
    case Nil  => doc.asRight
    case errs => NonEmptyList.fromListUnsafe(errs).asLeft
end validate

// 5.3.2 (10-2021)
// TODO: Implement this validator.

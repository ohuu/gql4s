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

/** Find the given fields type definition on the given type.
  *
  * @param fieldName
  *   The name of the field to find.
  * @param namedType
  *   The type to find the field on.
  * @return
  *   Some type definition if the field is found, None if not.
  */
private def findFieldTypeDef(
    fieldName: Name,
    namedType: NamedType,
    schema: TypeSystemDocument
): Option[TypeDefinition] =
  findTypeDef(namedType, schema)
    .flatMap {
      case ObjectTypeDefinition(_, _, _, fields) =>
        fields
          .find(_.name == fieldName)
          .flatMap(field => findTypeDef(NamedType(field.tpe.name), schema))

      case InterfaceTypeDefinition(_, _, _, fields) =>
        fields
          .find(_.name == fieldName)
          .flatMap(field => findTypeDef(NamedType(field.tpe.name), schema))

      case UnionTypeDefinition(_, _, members) =>
        members
          .find(findFieldTypeDef(fieldName, _, schema).isDefined)
          .flatMap(findTypeDef(_, schema))

      case _ => None
    }
end findFieldTypeDef

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

///////////////////////////////////////////////////////////////////////////////////////////////////
// Validators

// 5.2.1.1 (10-2021)
def operationNameUniqueness(doc: ExecutableDocument): Either[GqlError, ExecutableDocument] =
  val ops       = doc.collect { case x: OperationDefinition => x }
  val uniqueOps = ops.distinctBy(_.name)
  if uniqueOps.length == ops.length then doc.asRight else NameNotUnique.asLeft

// 5.2.2.1 (10-2021)
def loneAnonOperation(doc: ExecutableDocument): Either[GqlError, ExecutableDocument] =
  val ops      = doc.collect { case x: OperationDefinition => x }
  val namedOps = ops.filter(_.name.isDefined)
  val anonOps  = ops.filter(_.name.isEmpty)

  if anonOps.isEmpty then doc.asRight
  else if anonOps.length == 1 && namedOps.isEmpty then doc.asRight
  else AnonymousQueryNotAlone.asLeft

// 5.2.3.1 (10-2021)
// Checks to see if the used fragment has a single root
private def hasSingleRoot(
    frag: InlineFragment | FragmentSpread,
    fragDefs: List[FragmentDefinition]
): Boolean =
  frag match
    case InlineFragment(_, _, selects) => selects.length == 1
    case FragmentSpread(name, _) =>
      fragDefs.find(_.name.get == name) match
        case None       => false
        case Some(frag) => frag.selectionSet.length == 1

// TODO: need to stop introspection fields in subscription's root
def subscriptionSingleRoot(doc: ExecutableDocument): Either[GqlError, ExecutableDocument] =
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
    case Some(_) => SubscriptionHasMultipleRoots.asLeft
    case None    => doc.asRight
end subscriptionSingleRoot

// 5.3.1 (10-2021)
/** Checks whether the given field exists within the given type.
  *
  * @param fieldName
  *   The field we're looking for.
  * @param typeName
  *   The name of the type to search in.
  * @param schema
  *   The graphql schema.
  * @return
  *   Some MissingField error if the field cannot be found, None if it can.
  */
private def fieldExists(
    fieldName: Name,
    namedType: NamedType,
    schema: TypeSystemDocument
): Option[GqlError] =
  @tailrec
  def recurse(namedTypes: List[NamedType]): Boolean =
    namedTypes match
      case Nil => false
      case namedType :: tail =>
        val typeDef = findTypeDef(namedType, schema)

        typeDef match
          case Some(ObjectTypeDefinition(_, interfaces, _, fields)) =>
            if fields.find(_.name == fieldName).isDefined then true
            else recurse(interfaces ::: tail)

          case Some(InterfaceTypeDefinition(_, interfaces, _, fields)) =>
            if fields.find(_.name == fieldName).isDefined then true
            else recurse(interfaces ::: tail)

          case Some(UnionTypeDefinition(_, _, members)) =>
            recurse(members ::: tail)

          // The type that we're checking exists but isn't a type with fields
          // therefore the field can't exist so we just return false
          case _ => false
  end recurse

  if recurse(List(namedType)) then None
  else Some(MissingField(fieldName, Some(namedType)))
end fieldExists

/** Performs various validation steps on the selection sets. Bear in mind that this function will
  * not validate fragment definitions but will validate inline fragment definitions, you must call
  * validateFragmentDefinition as well as this function to fully validate an executable document.
  *
  * @param selectionSet
  *   The selectionSet to recursively validate.
  * @param typeName
  *   The name of the type that the selectionSet is within.
  * @param doc
  *   The query document. This is needed to find fragment definitions
  * @param schema
  *   The graphql schema.
  * @return
  *   Return a list of errors or Nil if there weren't any.
  */
private def validateSelectionSet(
    selectionSet: List[Selection],
    namedType: NamedType,
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
          case Field(_, name, _, _, fieldSelectionSet) =>
            // 5.3.1
            val accErrors = fieldExists(name, namedType, schema) match
              case None      => acc
              case Some(err) => err :: acc

            findFieldTypeDef(name, namedType, schema) match
              case None => MissingField(name) :: accErrors

              // We found it but it's a leaf type so we can't recurse
              case Some(_: ScalarTypeDefinition | _: EnumTypeDefinition) =>
                // 5.3.3
                if fieldSelectionSet.isEmpty then accErrors
                else IllegalSelection(name, namedType) :: accErrors

              // We found an object type so we need to recurse
              case Some(typeDef) =>
                // 5.3.3
                if fieldSelectionSet.isEmpty then MissingSelection(name, namedType) :: accErrors
                else
                  val typeAndSelection = fieldSelectionSet.map(NamedType(typeDef.name) -> _)
                  recurse(typeAndSelection ::: tail, accErrors)

          case InlineFragment(Some(namedType), _, fragSelectionSet) =>
            val typeAndSelection = fragSelectionSet.map(namedType -> _).toList
            recurse(typeAndSelection ::: tail, acc)

          // Type name has been ommited so this inline fragment has the same type as enclosing
          // context (e.g. the current namedType)
          case InlineFragment(None, _, fragSelectionSet) =>
            val typeAndSelection = fragSelectionSet.map(namedType -> _).toList
            recurse(typeAndSelection ::: tail, acc)

          // We have already validated fragment spreads by this stage so ignore, don't recurse
          case FragmentSpread(name, _) => acc
  end recurse

  recurse(selectionSet.map(namedType -> _))
end validateSelectionSet

/** */
private def validateOperationDefinition(
    opDef: OperationDefinition,
    schema: TypeSystemDocument
): List[GqlError] =
  findOperationTypeDef(opDef.operationType, schema) match
    case None => MissingOperationTypeDefinition(opDef.operationType) :: Nil
    case Some(typeDef) =>
      validateSelectionSet(opDef.selectionSet.toList, NamedType(typeDef.name), schema)

private def validateFragmentDefinition(
    fragDef: FragmentDefinition,
    schema: TypeSystemDocument
): List[GqlError] = validateSelectionSet(fragDef.selectionSet.toList, fragDef.on, schema)

def validate(
    doc: ExecutableDocument,
    schema: TypeSystemDocument
): Either[NonEmptyList[GqlError], ExecutableDocument] =
  val errors = doc
    .map {
      case o: OperationDefinition => validateOperationDefinition(o, schema)
      case o: FragmentDefinition  => validateFragmentDefinition(o, schema)
    }
    .reduce(_ ::: _)

  errors match
    case Nil  => doc.asRight
    case errs => NonEmptyList.fromListUnsafe(errs).asLeft
end validate

// 5.3.2 (10-2021)
// TODO: Implement this validator.

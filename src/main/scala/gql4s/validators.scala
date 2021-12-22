// Copyright (c) 2018-2021 by Oli Winks
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

private def findTypeDef(typeName: Name, schema: TypeSystemDocument): Option[TypeDefinition] =
  schema.collect { case t: TypeDefinition => t }.find(_.name == typeName)

private def findFieldTypeDef(
    fieldName: Name,
    typeName: Name,
    schema: TypeSystemDocument
): Option[TypeDefinition] =
  schema
    .collect { case t: TypeDefinition => t }
    .find(_.name == typeName)
    .flatMap {
      case ObjectTypeDefinition(name, _, _, fields) if name == typeName =>
        fields.find(_.name == fieldName).map(_.tpe.name).flatMap(findTypeDef(_, schema))

      case InterfaceTypeDefinition(name, _, _, fields) if name == typeName =>
        fields.find(_.name == fieldName).map(_.tpe.name).flatMap(findTypeDef(_, schema))

      case UnionTypeDefinition(name, _, members) if name == typeName =>
        members
          .map(_.name)
          .find(findFieldTypeDef(fieldName, _, schema).isDefined)
          .flatMap(findTypeDef(_, schema))

      case _ => None
    }
end findFieldTypeDef

private def findOperationTypeDef(
    opType: OperationType,
    schema: TypeSystemDocument
): Option[TypeDefinition] =
  val schemaDef = schema.collect { case s: SchemaDefinition => s }.headOption
  schemaDef match
    case Some(SchemaDefinition(_, roots)) =>
      roots
        .find(_.operationType == opType)
        .map(_.namedType.name)
        .flatMap(findTypeDef(_, schema))
    case None =>
      opType match
        case Query        => findTypeDef(Name("Query"), schema)
        case Mutation     => findTypeDef(Name("Mutation"), schema)
        case Subscription => findTypeDef(Name("Subscription"), schema)
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
    typeName: Name,
    schema: TypeSystemDocument
): Option[GqlError] =
  @tailrec
  def recurse(typeNames: List[NamedType]): Boolean =
    typeNames match
      case Nil => false
      case typeName :: tail =>
        val typeDef = schema.collect { case o: TypeDefinition => o }.find(_.name == typeName)

        typeDef match
          case Some(ObjectTypeDefinition(_, interfaces, _, fields)) =>
            if fields.find(_.name == fieldName).isDefined then true
            else recurse(tail ::: interfaces)

          case Some(InterfaceTypeDefinition(_, interfaces, _, fields)) =>
            if fields.find(_.name == fieldName).isDefined then true
            else recurse(tail ::: interfaces)

          case Some(UnionTypeDefinition(_, _, members)) =>
            recurse(tail ::: members)

          case _ => false
  end recurse

  if recurse(List(NamedType(typeName))) then None
  else Some(MissingField(fieldName, Some(typeName)))
end fieldExists

/** Checks a selection set to make sure all fields exist. This is a recursive function that checks
  * each new level of the selection hierarchy.
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
private def fieldsExist(
    selectionSet: List[Selection],
    typeName: Name,
    doc: ExecutableDocument,
    schema: TypeSystemDocument
): List[GqlError] =
  @tailrec
  def recurse(
      accSelectionSet: List[(Name, Selection)],
      acc: List[GqlError] = Nil
  ): List[GqlError] =
    accSelectionSet match
      case Nil => acc
      case (typeName, selection) :: tail =>
        selection match
          case Field(_, name, _, _, fieldSelectionSet) =>
            val accErrors = fieldExists(name, typeName, schema) match
              case None      => acc
              case Some(err) => err :: acc

            findFieldTypeDef(name, typeName, schema) match
              case None => MissingField(name) :: accErrors
              case Some(typeDef) =>
                val typeAndSelection = fieldSelectionSet.map(typeDef.name -> _)
                recurse(typeAndSelection ::: tail, accErrors)

          case InlineFragment(Some(NamedType(typeName)), _, fragSelectionSet) =>
            val typeAndSelection = fragSelectionSet.map(typeName -> _).toList
            recurse(typeAndSelection ::: tail, acc)

          // Type name has been ommited so this inline fragment has the same type as enclosing
          // context (e.g. the current typeName)
          case InlineFragment(None, _, fragSelectionSet) =>
            val typeAndSelection = fragSelectionSet.map(typeName -> _).toList
            recurse(typeAndSelection ::: tail, acc)

          case FragmentSpread(name, _) =>
            findFragDef(name, doc) match
              case None => MissingFragmentDefinition(name) :: acc
              case Some(FragmentDefinition(_, NamedType(typeName), _, fragSelectionSet)) =>
                val typeAndSelection = fragSelectionSet.map(typeName -> _).toList
                recurse(typeAndSelection ::: tail, acc)
  end recurse

  recurse(selectionSet.map(typeName -> _))
end fieldsExist

private def fieldsExist(
    opDef: OperationDefinition,
    doc: ExecutableDocument,
    schema: TypeSystemDocument
): List[GqlError] =
  findOperationTypeDef(opDef.operationType, schema) match
    case None => MissingOperationTypeDefinition(opDef.operationType) :: Nil
    case Some(typeDef) =>
      fieldsExist(opDef.selectionSet.toList, typeDef.name, doc, schema)

def fieldsExist(
    doc: ExecutableDocument,
    schema: TypeSystemDocument
): Either[NonEmptyList[GqlError], ExecutableDocument] =
  val errors = doc
    .collect { case o: OperationDefinition => o }
    .map(fieldsExist(_, doc, schema))
    .reduce(_ ::: _)

  errors match
    case Nil  => doc.asRight
    case errs => NonEmptyList.fromListUnsafe(errs).asLeft

// 5.3.3 (10-2021)
private def leafNodes(selection: Selection, onType: Name, schema: TypeSystemDocument): Boolean =
  ???

def leafNodes(
    doc: ExecutableDocument,
    schema: TypeSystemDocument
): Either[GqlError, ExecutableDocument] =
  doc.find {
    case FragmentDefinition(_, NamedType(onType), _, selectionSet) =>
      selectionSet.find(leafNodes(_, onType, schema)).isEmpty
    case OperationDefinition(opType, _, _, _, selectionSet) => ???
  }

  ???

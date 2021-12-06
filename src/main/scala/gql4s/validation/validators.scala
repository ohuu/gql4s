// Copyright (c) 2018-2021 by Oli Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s
package validation

import cats.data.NonEmptyList
import cats.implicits.*
import gql4s.parsers.*

import OperationType.*
import Selection.*
import Type.*
import scala.annotation.tailrec

// 5.2.1.1 (10-2021)
def operationNameUniqueness(doc: ExecutableDocument): Either[String, ExecutableDocument] =
  val ops       = doc.collect { case x: OperationDefinition => x }
  val uniqueOps = ops.distinctBy(_.name)
  if uniqueOps.length == ops.length then doc.asRight else "Fuck".asLeft

// 5.2.2.1 (10-2021)
def loneAnonOperation(doc: ExecutableDocument): Either[String, ExecutableDocument] =
  val ops      = doc.collect { case x: OperationDefinition => x }
  val namedOps = ops.filter(_.name.isDefined)
  val anonOps  = ops.filter(_.name.isEmpty)

  if anonOps.isEmpty then doc.asRight
  else if anonOps.length == 1 && namedOps.isEmpty then doc.asRight
  else "Shit".asLeft

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
def subscriptionSingleRoot(doc: ExecutableDocument): Either[String, ExecutableDocument] =
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
    case Some(_) => "Bollocks".asLeft
    case None    => doc.asRight
end subscriptionSingleRoot

// 5.3.1 (10-2021)
/** Finds the type of a field.
  *
  * @param fieldName
  *   The name of the field who's type we want.
  * @param typeName
  *   The name of the type that the field is defined in.
  * @param schema
  *   The graphql schema.
  *
  * TODO: Fields should have their types infered and stored in the Field ADT before this happens.
  * TODO: Optimise for performance.
  */
private def findFieldType(
    fieldName: Name,
    typeName: Name,
    schema: TypeSystemDocument
): Option[Name] =
  schema.toList.flatMap {
    case ObjectTypeDefinition(name, _, _, fields) if name == typeName =>
      fields.find(_.name == fieldName).map(_.tpe.name)

    case InterfaceTypeDefinition(name, _, _, fields) if name == typeName =>
      fields.find(_.name == fieldName).map(_.tpe.name)

    case UnionTypeDefinition(name, _, members) if name == typeName =>
      members.map(_.name).flatMap(findFieldType(fieldName, _, schema)).headOption

    case _ => None
  }.headOption

/** Checks whether the given field exists within the given type.
  *
  * @param fieldName
  *   The field we're looking for.
  * @param typeName
  *   The name of the type to search in.
  * @param schema
  *   The graphql schema.
  *
  * TODO: Make this tail-recursive
  */
private def fieldExists(fieldName: Name, typeName: Name, schema: TypeSystemDocument): Boolean =
  schema.find {
    case ObjectTypeDefinition(name, interfaces, _, fields) if name == typeName =>
      fields.find(_.name == fieldName).isDefined |
        interfaces.map(_.name).find(fieldExists(fieldName, _, schema)).isDefined

    case InterfaceTypeDefinition(name, interfaces, _, fields) if name == typeName =>
      fields.find(_.name == fieldName).isDefined |
        interfaces.map(_.name).find(fieldExists(fieldName, _, schema)).isDefined

    case UnionTypeDefinition(name, _, members) if name == typeName =>
      members.map(_.name).find(fieldExists(fieldName, _, schema)).isDefined

    case _ => false
  }.isDefined
end fieldExists

/** Checks a selection hierarchy to make sure all fields exist.
  *
  * @param selection
  *   The selection to check.
  * @param typeName
  *   The name of the type the selection is within.
  * @param schema
  *   The graphql schema.
  *
  * TODO: Performance optimization.
  */
private def fieldsExist(
    selection: Selection,
    typeName: Name,
    schema: TypeSystemDocument
): Boolean =
  selection match
    case Field(_, name, _, _, Nil) => fieldExists(name, typeName, schema)

    case Field(_, name, _, _, selectionSet) =>
      fieldExists(name, typeName, schema) &
        findFieldType(name, typeName, schema).flatMap { typeName =>
          selectionSet.find(fieldsExist(_, typeName, schema))
        }.isDefined

    case InlineFragment(tpe, _, selectionSet) =>
      selectionSet.find(fieldsExist(_, typeName, schema)).isDefined

    // This can be skipped because all fragment
    // definitions have been check by this time.
    case _: FragmentSpread => true

private def fieldsExist(fragDef: FragmentDefinition, schema: TypeSystemDocument): Boolean =
  fragDef.selectionSet.find(!fieldsExist(_, fragDef.on.name, schema)).isEmpty

private def fieldsExist(opDef: OperationDefinition, schema: TypeSystemDocument): Boolean =
  opDef.selectionSet.find(!fieldsExist(_, opDef.name.get, schema)).isEmpty

def fieldsExist(
    doc: ExecutableDocument,
    schema: TypeSystemDocument
): Either[String, ExecutableDocument] =
  val fragDefs = doc.collect { case f: FragmentDefinition => f }
  val opDefs   = doc.collect { case o: OperationDefinition => o }

  // check all fragment definitions first, only when
  // they have all passed check the operation definitions.
  val isValid =
    fragDefs.find(!fieldsExist(_, schema)).isEmpty &
      opDefs.find(!fieldsExist(_, schema)).isEmpty

  if isValid then doc.asRight else "Arse".asLeft
end fieldsExist

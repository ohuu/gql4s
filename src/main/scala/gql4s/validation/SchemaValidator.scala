// Copyright (c) 2021 by Oliver Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s
package validation

import GqlError.*
import Type.*
import scala.annotation.tailrec

object SchemaValidator:
  case class ValidSchema(
      schema: TypeSystemDocument,
      objTypes: Map[ObjectTypeDefinition, Set[InterfaceTypeDefinition]]
  ):
    export schema.*

  @tailrec
  def isValidImplementationFieldType(
      aFieldType: Type,
      bFieldType: Type,
      schema: TypeSystemDocument
  ): Option[GqlError] =
    (aFieldType, bFieldType) match
      case (NonNullType(a), NonNullType(b)) => isValidImplementationFieldType(a, b, schema)
      case (NonNullType(a), b)              => isValidImplementationFieldType(a, b, schema)
      case (ListType(a), ListType(b))       => isValidImplementationFieldType(a, b, schema)
      case (ListType(a), b)                 => Some(InvalidImplementation(a))
      case (a: NamedType, b: NamedType) =>
        val aTypeDef = schema.findTypeDef(a)
        val bTypeDef = schema.findTypeDef(b)

        (aTypeDef, bTypeDef) match
          case (Some(a), Some(b)) if a.name == b.name => None
          case (Some(aObj: ObjectTypeDefinition), Some(bUnion: UnionTypeDefinition)) =>
            if bUnion.unionMemberTypes.contains(a) then None
            else Some(InvalidImplementation(a))
          case (
                Some(aObj: (ObjectTypeDefinition | InterfaceTypeDefinition)),
                Some(_: InterfaceTypeDefinition)
              ) =>
            if aObj.interfaces.contains(b) then None
            else Some(InvalidImplementation(a))
          case _ => Some(InvalidImplementation(a))
      case (a, _) => Some(InvalidImplementation(a))
  end isValidImplementationFieldType

  /** Checks whether the given type {@a} is a valid implementation of {@b}
    *
    * See
    * https://github.com/graphql/graphql-spec/blame/October2021/spec/Section%203%20--%20Type%20System.md#L886-L906
    *
    * @param a
    *   The type we are checking correctly implements implementedType
    * @param implementedType
    *   The type that we're checking against
    */
  def isValidImplementation(
      a: ObjectTypeDefinition | InterfaceTypeDefinition,
      b: InterfaceTypeDefinition,
      schema: TypeSystemDocument
  ): List[GqlError] =
    // `a` MUST implement ALL interfaces that `b` implements
    val aInterfaceSet = a.interfaces.toSet
    val bInterfaceSet = b.interfaces.toSet
    val implErrs =
      if bInterfaceSet subsetOf aInterfaceSet then Nil
      else (bInterfaceSet diff aInterfaceSet).map(NonImplementedInterface(_)).toList

    // for each field in `b`
    val fieldErrs = b.fields.flatMap(bField =>
      // `a` must contain bField
      a.fields.find(_.name == bField.name) match
        case None         => List(MissingField(bField.name, NamedType(a.name)))
        case Some(aField) =>
          // get all the args in `b` and `a`
          val aArgs = aField.arguments
          val bArgs = bField.arguments

          // map all arguments in bField to the arguments in aField by name.
          val intersectingArgs = bArgs.map(bArg => bArg -> aArgs.find(_.name == bArg.name))

          // get all the extra arguments in aField
          val extraArgs = aArgs.filterNot(aArg => bArgs.exists(_.name == aArg.name))

          // aField must contain ALL args in bField
          val argExistsErrs = intersectingArgs
            .filter { case (_, aArg) => aArg.isEmpty }
            .map { case (bArg, _) => bArg.name }
            .map(MissingArgument(_, aField.name, NamedType(a.name)))

          // shared args must be the same type (invariant)
          val argTypeErrs = intersectingArgs
            .flatMap {
              case (_, None)          => None
              case (bArg, Some(aArg)) => Some(bArg -> aArg)
            }
            .filter { case (bArg, aArg) => bArg.tpe != aArg.tpe }
            .map { case (bArg, aArg) => IllegalType(aArg.tpe, Some(s"expected ${bArg.tpe}")) }

          // aField can have more args than bField but they must not be required fields
          val extraArgErrs = extraArgs
            .filter(arg => arg.tpe.isInstanceOf[NonNullType])
            .map(arg => IllegalType(arg.tpe, Some("Argument cannot be required")))

          // aField must return the same type or subtype (covariant) as bField
          val returnErrs = isValidImplementationFieldType(aField.tpe, bField.tpe, schema)

          argExistsErrs ::: argTypeErrs ::: extraArgErrs ::: returnErrs.toList
    )

    implErrs ::: fieldErrs
  end isValidImplementation

  def validateObjType(objType: ObjectTypeDefinition): List[GqlError] =
    // 3.6.1 Object types must define one or more fields
    val missingFieldsErr =
      if objType.fields.isEmpty then Some(MissingFields(NamedType(objType.name)))
      else None

    // 3.6.3 An object type may declare that it implements one or more unique interfaces.
    val uniqueInterfacesErrs = objType.interfaces
      .groupBy(_.n)
      .filter(_._2.length > 1)
      .map((name, _) => name)
      .map(DuplicateInterface(_))
      .toList

    // Interfaces must be defined

    ???
  end validateObjType

  def validate(schema: TypeSystemDocument): Either[List[GqlError], ValidSchema] =
    val objTypes = schema.definitions.collect { case o: ObjectTypeDefinition => o }

    ???
end SchemaValidator

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

  def validateNonNull(tpe: NonNullType): Option[GqlError] = tpe.tpe match
    case NonNullType(_) =>
      Some(IllegalType(tpe, Some("Non null types cannot wrap another non null type")))
    case _ => None

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

  /** Validates both object and interface types according to the `Type Validation` sections defined
    * in the GraphQL specification for object and interface types.
    *
    * @param tpe
    *   The object like type to validate.
    * @param schema
    *   The schema this type is defined in.
    * @return
    *   A list of errors, nil if no errors are found.
    */
  def validateObjLike(
      typeDef: ObjectTypeDefinition | InterfaceTypeDefinition,
      schema: TypeSystemDocument
  ): List[GqlError] =
    // 3.6.1 Object like types must define one or more fields
    val missingFieldsErr =
      if typeDef.fields.isEmpty then List(MissingFields(NamedType(typeDef.name)))
      else Nil

    // 3.6.2.1 Fields must have unique names within the Object type
    val uniqueFieldErrs =
      typeDef.fields
        .groupBy(_.name)
        .filter { case (_, fields) => fields.length > 1 }
        .map { case (name, _) => DuplicateField(name) }
        .toList

    // 3.6.2.2 Field names must not start with `__`
    val fieldNameErrs =
      typeDef.fields
        .map(_.name)
        .filter(_.name.startsWith("__"))
        .map(IllegalName.apply)

    // 3.6.2.3 Fields must return an output type
    val fieldTypeErrs =
      typeDef.fields
        .filterNot(f => schema.isOutputType(f.tpe))
        .map(_.tpe)
        .map(InvalidType.apply)

    // 3.6.2.4.1
    val fieldArgNameErrs =
      typeDef.fields
        .flatMap(_.arguments)
        .map(_.name)
        .filter(_.name.startsWith("__"))
        .map(IllegalName.apply)

    // 3.6.2.4.2
    val fieldArgTypeErrs =
      typeDef.fields
        .flatMap(_.arguments)
        .filterNot(f => schema.isInputType(f.tpe))
        .map(_.tpe)
        .map(InvalidType.apply)

    // 3.6.3 An object like type may declare that it implements one or more unique interfaces.
    //       If it's an interface type it may not implement itself
    val uniqueInterfacesErrs = typeDef.interfaces
      .groupBy(_.n)
      .filter(_._2.length > 1)
      .map((name, _) => name)
      .map(DuplicateInterface(_))
      .toList

    val selfImplErr =
      if typeDef.interfaces.exists(_.n == typeDef.name) then
        List(SelfImplementation(NamedType(typeDef.name)))
      else Nil

    // 3.6.4 An object type must be a super-set of all interfaces it implements
    val validImplErrs = typeDef.interfaces
      .map(typeDef => typeDef -> schema.findInterfaceTypeDef(typeDef))
      .flatMap {
        case (namedType, None)          => List(MissingTypeDefinition(namedType))
        case (_, Some(implemedtedType)) => isValidImplementation(typeDef, implemedtedType, schema)
      }

    missingFieldsErr :::
      uniqueFieldErrs :::
      fieldNameErrs :::
      fieldTypeErrs :::
      fieldArgNameErrs :::
      fieldArgTypeErrs :::
      uniqueInterfacesErrs :::
      selfImplErr :::
      validImplErrs
  end validateObjLike

  def validateUnion(typeDef: UnionTypeDefinition, schema: TypeSystemDocument): List[GqlError] =
    // 3.8.1 A Union type must include one or more unique member types.
    val emptyUnionErr =
      if typeDef.unionMemberTypes.isEmpty then List(MissingUnionMember(typeDef))
      else Nil

    // 3.8.2 The member types of a Union type must all be Object base types
    val memberTypeErrs = typeDef.unionMemberTypes.flatMap(memberType =>
      schema.findTypeDef(memberType) match
        case None                          => Some(MissingTypeDefinition(memberType))
        case Some(_: ObjectTypeDefinition) => None
        case Some(_) =>
          Some(IllegalType(memberType, Some("Only object types can be members of a union")))
    )

    emptyUnionErr ::: memberTypeErrs
  end validateUnion

  def validateEnum(typeDef: EnumTypeDefinition, schema: TypeSystemDocument): List[GqlError] =
    // 3.9.1 An Enum type must define one or more unique enum values.
    if typeDef.values.isEmpty then List(MissingEnumValue(typeDef))
    else Nil

  def validateInputObj(
      typeDef: InputObjectTypeDefinition,
      schema: TypeSystemDocument
  ): List[GqlError] =
    // 3.10.1 An Input Object type must define one or more input fields
    val fieldCountErr =
      if typeDef.fieldsDef.isEmpty then List(MissingFields(NamedType(typeDef.name)))
      else Nil

    // 3.10.2.1 Fields must have unique names within the Object type
    val uniqueFieldErrs =
      typeDef.fieldsDef
        .groupBy(_.name)
        .filter { case (_, fields) => fields.length > 1 }
        .map { case (name, _) => DuplicateField(name) }
        .toList

    // 3.10.2.2 Field names must not start with `__`
    val fieldNameErrs =
      typeDef.fieldsDef
        .map(_.name)
        .filter(_.name.startsWith("__"))
        .map(IllegalName.apply)

    // 3.10.2.3 Fields must return an output type
    val fieldTypeErrs =
      typeDef.fieldsDef
        .filterNot(f => schema.isInputType(f.tpe))
        .map(_.tpe)
        .map(InvalidType.apply)

    val children = typeDef.fieldsDef
      .map(_.tpe)
      .collect { case o: NonNullType => o }
      .map(_.tpe.name)
      .flatMap(schema.findInputObjTypeDef)
    val circularRefErrs = inputObjContainsCircularRef(typeDef, children, schema)

    fieldCountErr ::: uniqueFieldErrs ::: fieldNameErrs ::: fieldTypeErrs ::: circularRefErrs.toList
  end validateInputObj

  @tailrec
  def inputObjContainsCircularRef(
      a: InputObjectTypeDefinition,
      xs: List[InputObjectTypeDefinition],
      schema: TypeSystemDocument
  ): Option[GqlError] = xs match
    case Nil => None
    case x :: tail =>
      if a.name == x.name then Some(InputObjectContainsCycles(a.name))
      else
        val children = x.fieldsDef
          .map(_.tpe)
          .collect { case o: NonNullType => o }
          .map(_.tpe.name)
          .flatMap(schema.findInputObjTypeDef)
        inputObjContainsCircularRef(a, xs ::: children, schema)
  end inputObjContainsCircularRef

  def validate(schema: TypeSystemDocument): Either[List[GqlError], ValidSchema] =
    val errors = schema.definitions.toList.flatMap {
      case typeDef: ObjectLikeTypeDefinition  => validateObjLike(typeDef, schema)
      case typeDef: UnionTypeDefinition       => validateUnion(typeDef, schema)
      case typeDef: EnumTypeDefinition        => validateEnum(typeDef, schema)
      case typeDef: InputObjectTypeDefinition => validateInputObj(typeDef, schema)
      case _                                  => Nil
    }

    ???
end SchemaValidator

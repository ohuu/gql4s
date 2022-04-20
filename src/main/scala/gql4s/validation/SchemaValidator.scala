// Copyright (c) 2021 by Oliver Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s
package validation

import GqlError.*
import Type.*
import cats.implicits.*
import scala.annotation.tailrec
import scala.collection.mutable.LinkedHashMap
import cats.data.ValidatedNec
import java.util.UUID

object SchemaValidator:
  type Validated[T] = ValidatedNec[GqlError, T]
  case class ValidSchema(
      schema: TypeSystemDocument,
      objTypes: Map[ObjectTypeDefinition, Set[InterfaceTypeDefinition]]
  ):
    export schema.*

  /////////////////////////////////////////////////////////////////////////////////////////////////
  // Helper functions
  def buildInputObjDepGraph(
      inObjDefs: List[InputObjectTypeDefinition]
  ): DependencyGraph[InputObjectTypeDefinition] =
    LinkedHashMap.from(
      inObjDefs
        .map(inObj =>
          inObj -> inObj.fields
            .map(_.`type`.name)
            .flatMap(name => inObjDefs.find(_.name == name))
            .map(_.name)
            .toSet
        )
    )
  end buildInputObjDepGraph

  /////////////////////////////////////////////////////////////////////////////////////////////////
  // Validators
  def validateNotEmpty[T](ts: List[T]): Validated[List[T]] =
    if ts.isEmpty then StructureEmpty().invalidNec
    else ts.validNec

  def validateUniqueName[T <: HasName](ts: List[T]): Validated[List[T]] = ts
    .groupBy(_.name)
    .toList
    .traverse {
      case (_, occurance :: Nil) => occurance.validNec
      case (name, _)             => DuplicateName(name).invalidNec
    }

  def validateNames[T <: HasName](ts: List[T]): Validated[List[T]] = ts
    .traverse(t =>
      t.name match
        case name @ Name(text) if text.startsWith("__") => InvalidName(name).invalidNec
        case _                                          => t.validNec
    )

  def validateTypeExists[K <: TypeDefinition](t: Type)(using
      schema: TypeSystemDocument
  ): Validated[K] =
    schema.findTypeDef[K](t.name) match
      case Some(typeDef) => typeDef.validNec
      case None          => MissingTypeDefintion(t.name).invalidNec

  def validateTypes[T <: HasType](ts: List[T], validate: Type => Boolean): Validated[List[T]] = ts
    .traverse(t =>
      validate(t.`type`) match
        case true  => t.validNec
        case false => InvalidType(t.`type`).invalidNec
    )

  def validateNamedTypes[T <: HasName](ts: List[T], validate: Name => Boolean): Validated[List[T]] =
    ts
      .traverse(t =>
        validate(t.name) match
          case true  => t.validNec
          case false => InvalidNamedType(t.name).invalidNec
      )

  def validateSelfImplementation[T <: HasName & HasInterfaces](t: T): Validated[T] =
    if t.interfaces.exists(_.name == t.name)
    then SelfImplementation(NamedType(t.name)).invalidNec
    else t.validNec

  /** checks whether {@a} declares that it implements all interfaces that {@b} implements. */
  def validateDeclaredImplementations[T <: HasName & HasInterfaces](a: T, b: T): Validated[T] =
    val aInterfaceSet = a.interfaces.map(_.name).toSet
    val bInterfaceSet = b.interfaces.map(_.name).toSet
    if bInterfaceSet subsetOf aInterfaceSet then a.validNec
    else
      val missingImpls = (bInterfaceSet diff aInterfaceSet).toList
      MissingImplementations(a.name, missingImpls).invalidNec

  def validateInvariant[T <: HasType](a: T, b: T): Validated[T] =
    if a.`type` == b.`type` then a.validNec
    else InvalidType(a.`type`).invalidNec

  def validateCovariant(a: Type, b: Type)(using schema: TypeSystemDocument): Validated[Type] =
    @tailrec
    def validate(aType: Type, bType: Type): Boolean =
      (aType, bType) match
        case (NonNullType(a), NonNullType(b)) => validate(a, b)
        case (NonNullType(a), b)              => validate(a, b)
        case (ListType(a), ListType(b))       => validate(a, b)
        case (ListType(a), b)                 => false
        case (a: NamedType, b: NamedType) =>
          val aTypeDef = schema.findTypeDef[TypeDefinition](a.name)
          val bTypeDef = schema.findTypeDef[TypeDefinition](b.name)

          (aTypeDef, bTypeDef) match
            case (Some(a), Some(b)) if a.name == b.name => true
            case (Some(_: ObjectTypeDefinition), Some(bUnion: UnionTypeDefinition)) =>
              bUnion.unionMemberTypes.contains(a)
            case (Some(aObj: ObjectLikeTypeDefinition), Some(_: InterfaceTypeDefinition)) =>
              aObj.interfaces.contains(b)
            case _ => false
        case (a, _) => false
    end validate

    if validate(a, b) then a.validNec
    else InvalidType(a, Some(s"$a is not covariant with $b")).invalidNec
  end validateCovariant

  def validateArgs[T <: HasArgs](aField: T, bField: T)(using TypeSystemDocument): Validated[T] =
    val aArgs = aField.arguments
    val bArgs = bField.arguments

    val missingArgs      = bArgs.filter(bArg => aArgs.find(_.name == bArg.name).isDefined)
    val intersectingArgs = bArgs.mapFilter(bArg => aArgs.find(_.name == bArg.name).map(bArg -> _))
    val extraArgs        = aArgs.filter(aArg => bArgs.find(_.name == aArg.name).isEmpty)

    // aField must contain ALL args in bField
    // shared args must be the same type (invariant)
    val validateArgs = bArgs.traverse(bArg =>
      aArgs.find(_.name == bArg.name) match
        case None       => MissingArgument2(bArg.name).invalidNec
        case Some(aArg) => validateInvariant(aArg, bArg)
    )

    // aField can have more args than bField but they must not be required fields
    val validateExtraArgs = extraArgs
      .traverse(arg =>
        arg.`type` match
          case tpe: NonNullType => InvalidType(tpe).invalidNec
          case _                => arg.validNec
      )

    (validateArgs, validateExtraArgs).mapN((validArgs, validExtraArgs) => aField)
  end validateArgs

  def validateFields[T <: HasFields](a: T, b: T)(using TypeSystemDocument): Validated[T] = b.fields
    .traverse(bField =>
      a.fields.find(_.name == bField.name) match
        case None => MissingName(bField.name).invalidNec
        case Some(aField) =>
          (
            validateArgs(aField, bField),
            validateCovariant(aField.`type`, bField.`type`)
          ).mapN((validArgs, validReturnType) => aField)
    )
    .map(_ => a)
  end validateFields

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
  def validateImplementation[T <: HasName & HasFields & HasInterfaces](a: T, b: T)(using
      TypeSystemDocument
  ): Validated[T] =
    (
      validateDeclaredImplementations(a, b),
      validateFields(a, b)
    ).mapN((validImpls, validFields) => a)
  end validateImplementation

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
  def validateObjLike[T <: ObjectLikeTypeDefinition](typeDef: T)(using
      schema: TypeSystemDocument
  ): Validated[T] =
    // 3.6.1 Object like types must define one or more fields
    // 3.6.2.1 Fields must have unique names within the Object type
    // 3.6.2.2 Field names must not start with `__`
    // 3.6.2.3 Fields must return an output type
    // 3.6.2.4.1
    // 3.6.2.4.2
    // 3.6.3 An object like type may declare that it implements one or more unique interfaces.
    //       If it's an interface type it may not implement itself
    // 3.6.4 An object type must be a super-set of all interfaces it implements
    // respectively
    (
      validateNotEmpty(typeDef.fields),
      validateUniqueName(typeDef.fields),
      validateNames(typeDef.fields),
      validateTypes(typeDef.fields, schema.isOutputType),
      validateNames(typeDef.fields.flatMap(_.arguments)),
      validateTypes(typeDef.fields.flatMap(_.arguments), schema.isInputType),
      validateUniqueName(typeDef.interfaces),
      validateSelfImplementation(typeDef),
      typeDef.interfaces
        .traverse(validateTypeExists)
        .andThen(interfaces => interfaces.traverse(validateImplementation(typeDef, _)))
    ).mapN((v1, v2, v3, v4, v5, v6, v7, v8, validImplementations) => typeDef)

    // val validImplErrs = typeDef.interfaces
    //   .map(namedType => namedType -> schema.findTypeDef[InterfaceTypeDefinition](namedType.n))
    //   .flatMap {
    //     case (namedType, None)          => List(MissingTypeDefinition(namedType))
    //     case (_, Some(implemedtedType)) => isValidImplementation(typeDef, implemedtedType, schema)
    //   }
  end validateObjLike

  def validateUnion[T <: UnionTypeDefinition](typeDef: T)(using
      schema: TypeSystemDocument
  ): Validated[T] =
    // 3.8.1 A union type must include one or more unique member types.
    // 3.8.2 The member types of a union type must all be object base types
    // respectively
    (
      validateNotEmpty(typeDef.unionMemberTypes),
      validateUniqueName(typeDef.unionMemberTypes),
      validateNamedTypes(typeDef.unionMemberTypes, schema.isObjectType)
    ).mapN((v1, v2, v3) => typeDef)
  end validateUnion

  def validateEnum[T <: EnumTypeDefinition](typeDef: T)(using TypeSystemDocument): Validated[T] =
    // 3.9.1 An Enum type must define one or more unique enum values.
    (validateNotEmpty(typeDef.values), validateUniqueName(typeDef.values)).mapN((v1, v2) => typeDef)

  def validateInputObj[T <: InputObjectTypeDefinition](typeDef: T)(using
      schema: TypeSystemDocument
  ): Validated[T] =
    // validate
    // 3.10.1 An Input Object type must define one or more input args (fields)
    // 3.10.2.1 Arguments (fields) must have unique names within the Object type
    // 3.10.2.2 Argument (Field) names must not start with `__`
    // 3.10.2.3 Arguments (Fields) must return an output type
    // respectively
    (
      validateNotEmpty(typeDef.fields),
      validateUniqueName(typeDef.fields),
      validateNames(typeDef.fields),
      validateTypes(typeDef.fields, schema.isInputType)
    )
      .mapN((v1, v2, v3, v4) => typeDef)
  end validateInputObj

  def validate(schema: TypeSystemDocument): Validated[TypeSystemDocument] =
    given TypeSystemDocument = schema

    val objLikeDefs = schema.getTypeDef[ObjectLikeTypeDefinition]
    val unionDefs   = schema.getTypeDef[UnionTypeDefinition]
    val enumDefs    = schema.getTypeDef[EnumTypeDefinition]
    val inObjDefs   = schema.getTypeDef[InputObjectTypeDefinition]

    (
      objLikeDefs.traverse(validateObjLike),
      unionDefs.traverse(validateUnion),
      enumDefs.traverse(validateEnum),
      inObjDefs.traverse(validateInputObj),
      containsCycles(buildInputObjDepGraph(inObjDefs))
    ).mapN((validObjLikes, validUnions, validEnums, validInObjs, validGraph) => schema)
end SchemaValidator

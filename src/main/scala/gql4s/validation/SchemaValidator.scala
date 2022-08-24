// Copyright (c) 2021 by Oliver Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s
package validation

import cats.implicits.*
import scala.reflect.*
import scala.annotation.tailrec
import scala.collection.mutable.LinkedHashMap
import cats.data.ValidatedNec
import java.util.UUID

import errors.GqlError.*
import parsing.*
import parsing.Type.*

import TypeSystemDirectiveLocation as TSDL

object SchemaValidator:
  case class ValidSchema(
      schema: TypeSystemDocument,
      objTypes: Map[ObjectTypeDefinition, Set[InterfaceTypeDefinition]]
  ):
    export schema.*

  /////////////////////////////////////////////////////////////////////////////////////////////////
  // Helper functions
  def buildInputObjDepGraph[T <: InputObjectTypeDefinition](
      inObjDefs: List[T]
  ): DependencyGraph[T] =
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

  def buildDirDefDepGraph(
      dirDefs: List[DirectiveDefinition]
  )(using schema: TypeSystemDocument): DependencyGraph[DirectiveDefinition] =
    def toNameSet(dirs: List[Directive]): Set[Name] = dirs.map(_.name).toSet
    @tailrec
    def recurse(acc: List[HasDirectives], accDeps: Set[Name] = Set.empty): Set[Name] =
      acc match
        case Nil => accDeps
        case (ScalarTypeDefinition(_, dirs)) :: tail =>
          recurse(tail, toNameSet(dirs) union accDeps)
        case (InputValueDefinition(_, tpe, _, dirs)) :: tail =>
          val more = schema.findTypeDef[TypeDefinition](tpe.name).toList
          recurse(more ++ tail, toNameSet(dirs) union accDeps)
        case (FieldDefinition(_, args, tpe, dirs)) :: tail =>
          val more = args ++ schema.findTypeDef[TypeDefinition](tpe.name).toList
          recurse(more ++ tail, toNameSet(dirs) union accDeps)
        case (ObjectTypeDefinition(_, ints, dirs, fields)) :: tail =>
          val more = fields ++ ints.map(_.name).flatMap(schema.findTypeDef[TypeDefinition])
          recurse(more ++ tail, toNameSet(dirs) union accDeps)
        case (InterfaceTypeDefinition(_, ints, dirs, fields)) :: tail =>
          val more = fields ++ ints.map(_.name).flatMap(schema.findTypeDef[TypeDefinition])
          recurse(more ++ tail, toNameSet(dirs) union accDeps)
        case (UnionTypeDefinition(_, dirs, tpes)) :: tail =>
          val more = tpes.map(_.name).flatMap(schema.findTypeDef[TypeDefinition])
          recurse(more ++ tail, toNameSet(dirs) union accDeps)
        case (EnumValueDefinition(_, dirs)) :: tail =>
          recurse(tail, toNameSet(dirs) union accDeps)
        case (EnumTypeDefinition(_, dirs, vals)) :: tail =>
          recurse(vals ++ tail, toNameSet(dirs) union accDeps)
        case (InputObjectTypeDefinition(_, dirs, fields)) :: tail =>
          recurse(fields ++ tail, toNameSet(dirs) union accDeps)
        case _ :: tail =>
          recurse(tail, accDeps)
      end match
    end recurse

    LinkedHashMap.from(
      dirDefs.map(dirDef =>
        val argDirs = dirDef.arguments.flatMap(_.directives.map(_.name)).toSet
        val argTypeDirs = dirDef.arguments.flatMap(arg =>
          schema.findTypeDef[TypeDefinition](arg.`type`.name) match
            case None                          => Nil
            case Some(t: ScalarTypeDefinition) => Nil
        )
        dirDef -> (argDirs ++ argTypeDirs)
      )
    )
  end buildDirDefDepGraph

  /////////////////////////////////////////////////////////////////////////////////////////////////
  // Validators
  def validateNotEmpty[T](ts: List[T]): Validated[List[T]] =
    if ts.isEmpty then StructureEmpty().invalidNec
    else ts.validNec

  def validateNames[T <: HasName](ts: List[T]): Validated[List[T]] = ts
    .traverse(t =>
      t.name match
        case name @ Name(text) if text.startsWith("__") => InvalidName(name).invalidNec
        case _                                          => t.validNec
    )

  // TODO: Type test for K can't be performed at runtime. Is this a problem?
  def validateTypeExists[K <: TypeDefinition](
      t: Type
  )(using schema: TypeSystemDocument, tt: TypeTest[Any, K]): Validated[K] =
    schema.findTypeDef[K](t.name) match
      case Some(typeDef) => typeDef.validNec
      case None          => MissingDefinition(t.name).invalidNec

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

  def validateImplementationArgs[T <: HasArgs](aField: T, bField: T)(using
      TypeSystemDocument
  ): Validated[T] =
    val aArgs = aField.arguments
    val bArgs = bField.arguments

    val missingArgs      = bArgs.filter(bArg => aArgs.find(_.name == bArg.name).isDefined)
    val intersectingArgs = bArgs.mapFilter(bArg => aArgs.find(_.name == bArg.name).map(bArg -> _))
    val extraArgs        = aArgs.filter(aArg => bArgs.find(_.name == aArg.name).isEmpty)

    // aField must contain ALL args in bField
    // shared args must be the same type (invariant)
    val validateImplementationArgs = bArgs.traverse(bArg =>
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

    (validateImplementationArgs, validateExtraArgs).mapN((validArgs, validExtraArgs) => aField)
  end validateImplementationArgs

  def validateImplementationFields[T <: HasFields](a: T, b: T)(using
      TypeSystemDocument
  ): Validated[T] = b.fields
    .traverse(bField =>
      a.fields.find(_.name == bField.name) match
        case None => MissingName(bField.name).invalidNec
        case Some(aField) =>
          (
            validateImplementationArgs(aField, bField),
            validateCovariant(aField.`type`, bField.`type`)
          ).mapN((validArgs, validReturnType) => aField)
    )
    .map(_ => a)
  end validateImplementationFields

  /**
   * Checks whether the given type {@a} is a valid implementation of {@b}
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
      validateImplementationFields(a, b)
    ).mapN((_, _) => a)
  end validateImplementation

  def validateArgDefinition(argDef: InputValueDefinition)(using
      TypeSystemDocument
  ): Validated[InputValueDefinition] =
    validateDirectives(argDef.directives, TSDL.ARGUMENT_DEFINITION).map(_ => argDef)

  def validateFieldDefinition(fieldDef: FieldDefinition)(using
      TypeSystemDocument
  ): Validated[FieldDefinition] =
    (
      validateDirectives(fieldDef.directives, TSDL.FIELD_DEFINITION),
      fieldDef.arguments.traverse(validateArgDefinition)
    ).mapN((_, _) => fieldDef)
  end validateFieldDefinition

  /**
   * Validates both object and interface types according to the `Type Validation` sections defined
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
      schema: TypeSystemDocument,
      tt: TypeTest[Any, T]
  ): Validated[T] =
    (
      // 3.6.1 Object like types must define one or more fields
      validateNotEmpty(typeDef.fields),

      // 3.6.2.1 Fields must have unique names within the Object type
      validateUniqueName(typeDef.fields),

      // 3.6.2.2 Field names must not start with `__`
      validateNames(typeDef.fields),

      // 3.6.2.3 Fields must return an output type
      validateTypes(typeDef.fields, schema.isOutputType),

      // 3.6.2.4.1
      validateNames(typeDef.fields.flatMap(_.arguments)),

      // 3.6.2.4.2
      validateTypes(typeDef.fields.flatMap(_.arguments), schema.isInputType),

      // 3.6.3 An object like type may declare that it implements one or more unique interfaces.
      //       If it's an interface type it may not implement itself
      validateUniqueName(typeDef.interfaces),
      validateSelfImplementation(typeDef),

      // 3.6.4 An object type must be a super-set of all interfaces it implements
      typeDef.interfaces
        .traverse(validateTypeExists[ObjectLikeTypeDefinition])
        .andThen(interfaces => interfaces.traverse(validateImplementation(typeDef, _))),
      typeDef.fields.traverse(validateFieldDefinition)
    ).mapN((_, _, _, _, _, _, _, _, _, _) => typeDef)
  end validateObjLike

  def validateObjTypeDef[T <: ObjectTypeDefinition](
      typeDef: T
  )(using TypeSystemDocument, TypeTest[Any, T]): Validated[T] =
    (
      validateDirectives(typeDef.directives, TSDL.OBJECT),
      validateObjLike(typeDef)
    ).mapN((_, _) => typeDef)
  end validateObjTypeDef

  def validateInterfaceTypeDef[T <: InterfaceTypeDefinition](
      typeDef: T
  )(using TypeSystemDocument, TypeTest[Any, T]): Validated[T] =
    (
      validateDirectives(typeDef.directives, TSDL.INTERFACE),
      validateObjLike(typeDef)
    ).mapN((_, _) => typeDef)
  end validateInterfaceTypeDef

  def validateUnion[T <: UnionTypeDefinition](typeDef: T)(using
      TypeSystemDocument
  ): Validated[T] =
    (
      // 3.8.1 A union type must include one or more unique member types.
      validateNotEmpty(typeDef.unionMemberTypes),
      validateUniqueName(typeDef.unionMemberTypes),

      // 3.8.2 The member types of a union type must all be object base types
      validateNamedTypes(typeDef.unionMemberTypes, summon[TypeSystemDocument].isObjectType),

      // validate directives
      validateDirectives(typeDef.directives, TSDL.UNION)
    ).mapN((_, _, _, _) => typeDef)
  end validateUnion

  def validateEnum[T <: EnumTypeDefinition](typeDef: T)(using
      TypeSystemDocument
  ): Validated[T] =
    (
      // 3.9.1 An Enum type must define one or more unique enum values.
      validateNotEmpty(typeDef.values),
      validateUniqueName(typeDef.values),

      // validate directives
      validateDirectives(typeDef.directives, TSDL.ENUM),
      validateDirectives(typeDef.values.flatMap(_.directives), TSDL.ENUM_VALUE)
    ).mapN((_, _, _, _) => typeDef)
  end validateEnum

  def validateInputObj[T <: InputObjectTypeDefinition](typeDef: T)(using
      TypeSystemDocument
  ): Validated[T] =
    (
      // 3.10.1 An Input Object type must define one or more input args (fields)
      validateNotEmpty(typeDef.fields),

      // 3.10.2.1 Arguments (fields) must have unique names within the Object type
      validateUniqueName(typeDef.fields),

      // 3.10.2.2 Argument (Field) names must not start with `__`
      validateNames(typeDef.fields),

      // 3.10.2.3 Arguments (Fields) must return an output type
      validateTypes(typeDef.fields, summon[TypeSystemDocument].isInputType),

      // validate directives
      validateDirectives(typeDef.directives, TSDL.INPUT_OBJECT),
      validateDirectives(typeDef.fields.flatMap(_.directives), TSDL.INPUT_FIELD_DEFINITION)
    )
      .mapN((_, _, _, _, _, _) => typeDef)
  end validateInputObj

  def validateScalarTypeDefinition(typeDef: ScalarTypeDefinition)(using
      TypeSystemDocument
  ): Validated[ScalarTypeDefinition] =
    validateDirectives(typeDef.directives, TSDL.SCALAR).map(_ => typeDef)

  def validateSchemaDefinition(schemaDef: SchemaDefinition)(using
      TypeSystemDocument
  ): Validated[SchemaDefinition] =
    validateDirectives(schemaDef.directives, TSDL.SCHEMA).map(_ => schemaDef)

  def validateDirectiveDefinition(dirDef: DirectiveDefinition)(using
      TypeSystemDocument
  ): Validated[DirectiveDefinition] = ???

  def validate(schema: TypeSystemDocument): Validated[TypeSystemDocument] =
    given TypeSystemDocument = schema

    val schemaDefs        = schema.getTypeDef[SchemaDefinition]
    val directiveDefs     = schema.getTypeDef[DirectiveDefinition]
    val objTypeDefs       = schema.getTypeDef[ObjectTypeDefinition]
    val interfaceTypeDefs = schema.getTypeDef[InterfaceTypeDefinition]
    val unionDefs         = schema.getTypeDef[UnionTypeDefinition]
    val enumDefs          = schema.getTypeDef[EnumTypeDefinition]
    val inObjDefs         = schema.getTypeDef[InputObjectTypeDefinition]
    val scalarTypeDefs    = schema.getTypeDef[ScalarTypeDefinition]

    (
      schemaDefs.traverse(validateSchemaDefinition),
      directiveDefs.traverse(validateDirectiveDefinition),
      objTypeDefs.traverse(validateObjTypeDef),
      interfaceTypeDefs.traverse(validateInterfaceTypeDef),
      scalarTypeDefs.traverse(validateScalarTypeDefinition),
      unionDefs.traverse(validateUnion),
      enumDefs.traverse(validateEnum),
      inObjDefs.traverse(validateInputObj),
      containsCycles(buildInputObjDepGraph(inObjDefs))
    ).mapN((_, _, _, _, _, _, _, _, _) => schema)
end SchemaValidator

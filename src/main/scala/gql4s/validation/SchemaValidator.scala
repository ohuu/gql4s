// Copyright (c) 2021 by Oliver Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s
package validation

import java.util.UUID
import scala.annotation.tailrec
import scala.collection.mutable.LinkedHashMap
import scala.reflect.*

import cats.data.ValidatedNec
import cats.implicits.*

import errors.GqlError.*
import parsing.*
import parsing.Type.*
import parsing.TypeSystemDirectiveLocation as TSDL
import validation.Topo.*

object SchemaValidator:
    def validateNotEmpty[T](ts: List[T]): Validated[List[T]] =
        if ts.isEmpty then StructureEmpty().invalidNec
        else ts.validNec

    def validateName(n: Name): Validated[Name] =
        if n.text.startsWith("__") then InvalidName(n).invalidNec else n.validNec

    def validateObjLikeTypeExists(name: Name)(using ctx: SchemaContext): Validated[ObjectLikeTypeDefinition] =
        ctx.getTypeDef(name) match
            case Some(typeDef: (ObjectTypeDefinition | InterfaceTypeDefinition)) => typeDef.validNec
            case _                                                               => MissingDefinition(name).invalidNec

    def validateType(t: Type, test: Type => Boolean): Validated[Type] =
        if test(t) then t.validNec else InvalidType(t).invalidNec

    def validateNamedTypes[T <: HasName](ts: List[T], validate: Name => Boolean): Validated[List[T]] =
        ts
            .traverse { t =>
                validate(t.name) match
                    case true  => t.validNec
                    case false => InvalidNamedType(t.name).invalidNec
            }

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

    def validateImplementationArgs[T <: HasArgs](aField: T, bField: T)(using SchemaContext): Validated[T] =
        val aArgs = aField.arguments
        val bArgs = bField.arguments

        val missingArgs      = bArgs.filter(bArg => aArgs.find(_.name == bArg.name).isDefined)
        val intersectingArgs = bArgs.mapFilter(bArg => aArgs.find(_.name == bArg.name).map(bArg -> _))
        val extraArgs        = aArgs.filter(aArg => bArgs.find(_.name == aArg.name).isEmpty)

        // aField must contain ALL args in bField
        // shared args must be the same type (invariant)
        val validateImplementationArgs = bArgs.traverse { bArg =>
            aArgs.find(_.name == bArg.name) match
                case None       => MissingArgument2(bArg.name).invalidNec
                case Some(aArg) => validateInvariant(aArg, bArg)
        }

        // aField can have more args than bField but they must not be required fields
        val validateExtraArgs = extraArgs.traverse { arg =>
            arg.`type` match
                case tpe: NonNullType => InvalidType(tpe).invalidNec
                case _                => arg.validNec
        }

        (validateImplementationArgs, validateExtraArgs).mapN((validArgs, validExtraArgs) => aField)
    end validateImplementationArgs

    def validateImplementationFields[T <: HasFields](a: T, b: T)(using SchemaContext): Validated[T] = b.fields
        .traverse { bField =>
            a.fields.find(_.name == bField.name) match
                case None => MissingName(bField.name).invalidNec
                case Some(aField) =>
                    (
                        validateImplementationArgs(aField, bField),
                        validateCovariant(aField.`type`, bField.`type`)
                    ).mapN((validArgs, validReturnType) => aField)
        }
        .map(_ => a)

    /** Checks whether the given type {@a} is a valid implementation of {@b}
      *
      * See https://github.com/graphql/graphql-spec/blame/October2021/spec/Section%203%20--%20Type%20System.md#L886-L906
      *
      * @param a
      *   The type we are checking correctly implements implementedType
      * @param implementedType
      *   The type that we're checking against
      */
    def validateImplementation[T <: HasName & HasFields & HasInterfaces](a: T, b: T)(using
        SchemaContext
    ): Validated[T] =
        (
            validateDeclaredImplementations(a, b),
            validateImplementationFields(a, b)
        ).mapN((_, _) => a)
    end validateImplementation

    def validateArgDefinition(argDef: InputValueDefinition)(using SchemaContext): Validated[InputValueDefinition] =
        validateDirectives(argDef.directives, TSDL.ARGUMENT_DEFINITION).map(_ => argDef)

    def validateFieldDefinition(fieldDef: FieldDefinition)(using SchemaContext): Validated[FieldDefinition] =
        (
            validateDirectives(fieldDef.directives, TSDL.FIELD_DEFINITION),
            fieldDef.arguments.traverse(validateArgDefinition)
        ).mapN((_, _) => fieldDef)

    /** Validates both object and interface types according to the `Type Validation` sections defined in the GraphQL
      * specification for object and interface types.
      *
      * @param tpe
      *   The object like type to validate.
      * @param schema
      *   The schema this type is defined in.
      * @return
      *   A list of errors, nil if no errors are found.
      */
    def validateObjLike(typeDef: ObjectLikeTypeDefinition)(using SchemaContext): Validated[ObjectLikeTypeDefinition] =
        (
            // 3.6.1 Object like types must define one or more fields
            validateNotEmpty(typeDef.fields),

            // 3.6.2.1 Fields must have unique names within the Object type
            validateUniqueName(typeDef.fields),

            // 3.6.2.2 Field names must not start with `__`
            typeDef.fields.map(_.name).traverse(validateName),

            // 3.6.2.3 Fields must return an output type
            typeDef.fields.map(_.`type`).traverse(validateType(_, isOutputType)),

            // 3.6.2.4.1
            typeDef.fields.flatMap(_.arguments).map(_.name).traverse(validateName),

            // 3.6.2.4.2
            typeDef.fields
                .flatMap(_.arguments)
                .map(_.`type`)
                .traverse(validateType(_, isInputType)),

            // 3.6.3 An object like type may declare that it implements one or more unique interfaces.
            //       If it's an interface type it may not implement itself
            validateUniqueName(typeDef.interfaces),
            validateSelfImplementation(typeDef),

            // 3.6.4 An object type must be a super-set of all interfaces it implements
            typeDef.interfaces
                .map(_.name)
                .traverse(validateObjLikeTypeExists)
                .andThen(interfaces => interfaces.traverse(validateImplementation(typeDef, _))),
            typeDef.fields.traverse(validateFieldDefinition)
        ).mapN((_, _, _, _, _, _, _, _, _, _) => typeDef)
    end validateObjLike

    def validateObjTypeDef(typeDef: ObjectTypeDefinition)(using SchemaContext): Validated[ObjectTypeDefinition] =
        (
            validateDirectives(typeDef.directives, TSDL.OBJECT),
            validateObjLike(typeDef)
        ).mapN((_, _) => typeDef)

    def validateInterfaceTypeDef(
        typeDef: InterfaceTypeDefinition
    )(using SchemaContext): Validated[InterfaceTypeDefinition] =
        (
            validateDirectives(typeDef.directives, TSDL.INTERFACE),
            validateObjLike(typeDef)
        ).mapN((_, _) => typeDef)

    def validateUnionTypeDef(typeDef: UnionTypeDefinition)(using SchemaContext): Validated[UnionTypeDefinition] =
        (
            // 3.8.1 A union type must include one or more unique member types.
            validateNotEmpty(typeDef.unionMemberTypes),
            validateUniqueName(typeDef.unionMemberTypes),

            // 3.8.2 The member types of a union type must all be object base types
            validateNamedTypes(typeDef.unionMemberTypes, isObjectType),

            // validate directives
            validateDirectives(typeDef.directives, TSDL.UNION)
        ).mapN((_, _, _, _) => typeDef)

    def validateEnumTypeDef(typeDef: EnumTypeDefinition)(using SchemaContext): Validated[EnumTypeDefinition] =
        (
            // 3.9.1 An Enum type must define one or more unique enum values.
            validateNotEmpty(typeDef.values),
            validateUniqueName(typeDef.values),

            // validate directives
            validateDirectives(typeDef.directives, TSDL.ENUM),
            validateDirectives(typeDef.values.flatMap(_.directives), TSDL.ENUM_VALUE)
        ).mapN((_, _, _, _) => typeDef)

    def validateInputObjTypeDef(typeDef: InputObjectTypeDefinition)(using
        SchemaContext
    ): Validated[InputObjectTypeDefinition] =
        (
            // 3.10.1 An Input Object type must define one or more input args (fields)
            validateNotEmpty(typeDef.fields),

            // 3.10.2.1 Arguments (fields) must have unique names within the Object type
            validateUniqueName(typeDef.fields),

            // 3.10.2.2 Argument (Field) names must not start with `__`
            typeDef.fields.map(_.name).traverse(validateName),

            // 3.10.2.3 Arguments (Fields) must return an output type
            typeDef.fields.map(_.`type`).traverse(validateType(_, isInputType)),

            // validate directives
            validateDirectives(typeDef.directives, TSDL.INPUT_OBJECT),
            validateDirectives(typeDef.fields.flatMap(_.directives), TSDL.INPUT_FIELD_DEFINITION)
        )
            .mapN((_, _, _, _, _, _) => typeDef)

    def validateScalarTypeDefinition(typeDef: ScalarTypeDefinition)(using
        SchemaContext
    ): Validated[ScalarTypeDefinition] =
        validateDirectives(typeDef.directives, TSDL.SCALAR).map(_ => typeDef)

    def validateSchemaDefinition(schemaDef: SchemaDefinition)(using SchemaContext): Validated[SchemaDefinition] =
        validateDirectives(schemaDef.directives, TSDL.SCHEMA).map(_ => schemaDef)

    def validateDirectiveDefinitions(dirDefs: List[DirectiveDefinition])(using
        ctx: SchemaContext
    ): Validated[List[DirectiveDefinition]] =
        ctx.dirDeps.topo match
            case NoCycles(_) =>
                (
                    dirDefs.map(_.name).traverse(validateName),
                    dirDefs.flatMap(_.arguments).map(_.name).traverse(validateName),
                    dirDefs.flatMap(_.arguments).map(_.`type`).traverse(validateType(_, isInputType))
                ).mapN((_, _, _) => dirDefs)
            case HasCycles(cycles) => CyclesDetected(cycles).invalidNec

    def validate(schema: TypeSystemDocument): Validated[TypeSystemDocument] =
        val ctx             = SchemaContext(schema)
        given SchemaContext = ctx

        val validatedInObjDefs = ctx.inObjDeps.topo match
            case NoCycles(order)   => order.validNec
            case HasCycles(cycles) => CyclesDetected(cycles).invalidNec

        (
            validateDirectiveDefinitions(ctx.directiveDefs),
            ctx.getSchemaDef.traverse(validateSchemaDefinition),
            ctx.objTypeDefs.traverse(validateObjTypeDef),
            ctx.ifTypeDefs.traverse(validateInterfaceTypeDef),
            ctx.scalarTypeDefs.traverse(validateScalarTypeDefinition),
            ctx.unionTypeDefs.traverse(validateUnionTypeDef),
            ctx.enumTypeDefs.traverse(validateEnumTypeDef),
            ctx.inObjTypeDefs.traverse(validateInputObjTypeDef),
            validatedInObjDefs
        ).mapN((_, _, _, _, _, _, _, _, _) => schema)
end SchemaValidator

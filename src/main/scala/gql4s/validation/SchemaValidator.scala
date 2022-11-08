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
import parsing.Value.*
import validation.Topo.*

object SchemaValidator:
    def validateName(n: Name): Validated[Name] =
        if n.text.startsWith("__") then IllegalName(n).invalidNec else n.validNec

    def validateInterfaceTypeDefExists(`type`: NamedType)(using
        ctx: SchemaContext
    ): Validated[InterfaceTypeDefinition] =
        ctx.getTypeDef(`type`.name) match
            case Some(typeDef: InterfaceTypeDefinition) => typeDef.validNec
            case _                                      => InterfaceDefinitionMissing(`type`.name).invalidNec

    def validateSelfImplementation(typeDef: InterfaceTypeDefinition): Validated[InterfaceTypeDefinition] =
        if typeDef.interfaces.exists(_.name == typeDef.name) then
            InterfaceDefinitionImplementsSelf(typeDef.name).invalidNec
        else typeDef.validNec

    /** checks whether {@a} declares that it implements all interfaces that {@b} implements. */
    def validateDeclaredImplementations(
        a: ObjectLikeTypeDefinition,
        b: InterfaceTypeDefinition
    ): Validated[ObjectLikeTypeDefinition] =
        val aInterfaceSet = a.interfaces.toSet
        val bInterfaceSet = b.interfaces.toSet
        if bInterfaceSet subsetOf aInterfaceSet then a.validNec
        else
            val missingImpls = (bInterfaceSet diff aInterfaceSet).toList.map(_.name)
            missingImpls.traverse(name => InterfaceImplementationMissing(a.name, name).invalidNec).map(_ => a)

    def validateInvariant(a: InputValueDefinition, b: InputValueDefinition): Validated[InputValueDefinition] =
        if a.`type` == b.`type` then a.validNec
        else TypeNotInvariant(a.`type`, b.`type`).invalidNec

    def validateFieldHasArgs(aField: FieldDefinition, bField: FieldDefinition)(using
        SchemaContext
    ): Validated[FieldDefinition] =
        val aArgs = aField.arguments
        val bArgs = bField.arguments

        val missingArgs      = bArgs.filter(bArg => aArgs.find(_.name == bArg.name).isDefined)
        val intersectingArgs = bArgs.mapFilter(bArg => aArgs.find(_.name == bArg.name).map(bArg -> _))
        val extraArgs        = aArgs.filter(aArg => bArgs.find(_.name == aArg.name).isEmpty)

        // aField must contain ALL args in bField
        // shared args must be the same type (invariant)
        val validatedImplementationArgs = bArgs.traverse { bArg =>
            aArgs.find(_.name == bArg.name) match
                case None       => RequiredArgumentMissing(bArg.name, aField.name).invalidNec
                case Some(aArg) => validateInvariant(aArg, bArg)
        }

        // aField can have more args than bField but they must not be required fields
        val validatedExtraArgs = extraArgs.traverse { arg =>
            arg.`type` match
                case tpe: NonNullType => ArgumentCannotBeRequired(aField.name, arg.name).invalidNec
                case _                => arg.validNec
        }

        (validatedImplementationArgs, validatedExtraArgs).mapN((validArgs, validExtraArgs) => aField)
    end validateFieldHasArgs

    /** Validates that an implementing type contains all the fields of the types it implements
      */
    def validateImplementationFields(a: ObjectLikeTypeDefinition, b: InterfaceTypeDefinition)(using
        SchemaContext
    ): Validated[ObjectLikeTypeDefinition] = b.fields
        .traverse { bField =>
            a.fields.find(_.name == bField.name) match
                case None => FieldMissing(a.name, bField.name).invalidNec
                case Some(aField) =>
                    (
                        validateFieldHasArgs(aField, bField),
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
      * @param b
      *   The type that we're checking against
      */
    def validateImplementation(a: ObjectLikeTypeDefinition, b: InterfaceTypeDefinition)(using
        SchemaContext
    ): Validated[ObjectLikeTypeDefinition] =
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
            if typeDef.fields.isEmpty then NoFields(typeDef.name).invalidNec else typeDef.validNec,

            // 3.6.2.1 Fields must have unique names within the Object type
            validateUniqueName(typeDef.fields)(DuplicateFields(_)),

            // 3.6.2.2 Field names must not start with `__`
            typeDef.fields.map(_.name).traverse(validateName),

            // 3.6.2.3 Fields must return an output type
            typeDef.fields.map(_.`type`).traverse(validateOutputType),

            // 3.6.2.4.1
            typeDef.fields.flatMap(_.arguments).map(_.name).traverse(validateName),

            // 3.6.2.4.2
            typeDef.fields
                .flatMap(_.arguments)
                .map(_.`type`)
                .traverse(validateInputType),

            // 3.7.3 extended interfaces must be unique
            validateUniqueName(typeDef.interfaces)(DuplicateInterfaceImpls(_)),

            // 3.6.3 or 3.6.4
            typeDef match
                // 3.6.3 An interface may declare that it implements one or more interfaces.
                //       However, an interface may not implement itself
                case ifTypeDef: InterfaceTypeDefinition => validateSelfImplementation(ifTypeDef)

                // 3.6.4 An object type must be a super-set of all interfaces it implements
                case objTypeDef: ObjectTypeDefinition =>
                    objTypeDef.interfaces
                        .traverse(validateInterfaceTypeDefExists)
                        .andThen(interfaces =>
                            (
                                interfaces.traverse(validateImplementation(objTypeDef, _)),
                                objTypeDef.fields.traverse(validateFieldDefinition)
                            ).mapN((_, _) => objTypeDef)
                        )
        ).mapN((_, _, _, _, _, _, _, _) => typeDef)
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
            if typeDef.unionMemberTypes.isEmpty then NoUnionMembers(typeDef.name).invalidNec else typeDef.validNec,
            validateUniqueName(typeDef.unionMemberTypes)(DuplicateUnionMembers(_)),

            // 3.8.2 The member types of a union type must all be object base types
            typeDef.unionMemberTypes.traverse(validateObjectType),

            // validate directives
            validateDirectives(typeDef.directives, TSDL.UNION)
        ).mapN((_, _, _, _) => typeDef)

    def validateEnumTypeDef(typeDef: EnumTypeDefinition)(using SchemaContext): Validated[EnumTypeDefinition] =
        (
            // 3.9.1 An Enum type must define one or more unique enum values.
            if typeDef.values.isEmpty then NoEnumValues(typeDef.name).invalidNec else typeDef.validNec,
            validateUniqueName(typeDef.values)(DuplicateEnumValues(_)),

            // validate directives
            validateDirectives(typeDef.directives, TSDL.ENUM),
            validateDirectives(typeDef.values.flatMap(_.directives), TSDL.ENUM_VALUE)
        ).mapN((_, _, _, _) => typeDef)

    def validateInputObjTypeDefs(using ctx: SchemaContext): Validated[List[Name]] =
        ctx.inObjDeps.topo match
            case NoCycles(order)   => order.validNec
            case HasCycles(cycles) => InputObjectTypeDefinitionHasCyclicalDependency(cycles).invalidNec

    def validateInputObjTypeDef(typeDef: InputObjectTypeDefinition)(using
        SchemaContext
    ): Validated[InputObjectTypeDefinition] =
        (
            // 3.10.1 An Input Object type must define one or more input args (fields)
            if typeDef.fields.isEmpty then NoFields(typeDef.name).invalidNec else typeDef.validNec,

            // 3.10.2.1 Arguments (fields) must have unique names within the Object type
            validateUniqueName(typeDef.fields)(DuplicateFields(_)),

            // 3.10.2.2 Argument (Field) names must not start with `__`
            typeDef.fields.map(_.name).traverse(validateName),

            // 3.10.2.3 Arguments (Fields) must return an output type
            typeDef.fields.map(_.`type`).traverse(validateInputType),

            // validate directives
            validateDirectives(typeDef.directives, TSDL.INPUT_OBJECT),
            validateDirectives(typeDef.fields.flatMap(_.directives), TSDL.INPUT_FIELD_DEFINITION)
        ).mapN((_, _, _, _, _, _) => typeDef)

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
                    dirDefs.flatMap(_.arguments).map(_.`type`).traverse(validateInputType)
                ).mapN((_, _, _) => dirDefs)
            case HasCycles(cycles) => DirectiveDefinitionHasCyclicalDependency(cycles).invalidNec

    def validate(schema: TypeSystemDocument): Validated[TypeSystemDocument] =
        given ctx: SchemaContext = SchemaContext(schema)

        (
            validateDirectiveDefinitions(ctx.directiveDefs),
            ctx.getSchemaDef.traverse(validateSchemaDefinition),
            ctx.objTypeDefs.traverse(validateObjTypeDef),
            ctx.ifTypeDefs.traverse(validateInterfaceTypeDef),
            ctx.scalarTypeDefs.traverse(validateScalarTypeDefinition),
            ctx.unionTypeDefs.traverse(validateUnionTypeDef),
            ctx.enumTypeDefs.traverse(validateEnumTypeDef),
            ctx.inObjTypeDefs.traverse(validateInputObjTypeDef),
            validateInputObjTypeDefs
        ).mapN((_, _, _, _, _, _, _, _, _) => schema)
end SchemaValidator

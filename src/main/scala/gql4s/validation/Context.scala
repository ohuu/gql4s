package gql4s
package validation

import scala.annotation.tailrec

import cats.data.Kleisli

import parsing.*
import parsing.OperationType.*
import parsing.Type.NamedType
import parsing.Value.*

/** Symbol Table
  * ------------
  *
  * Symbol tables link names to objects in the AST and in turn, those objects to other ojects.
  *
  * The AST contains many object types which are associated with other objects. For example VariableDefinitions and
  * TypeDefinitions.
  *
  * The problem is that when validating the AST you often need access to these related objects but don't have them to
  * hand.
  *
  * The symbol table exist not to validate the AST but to provide a snapshot of what the AST is actually like. It's a
  * support object for all validation functions.
  *
  * These are the mapped objects (grouped by type) Name -> Variable Variable -> VariableDefinition VariableDefinition ->
  * TypeDefinition
  *
  * ```
  * Name -> InputValueDefinition
  * InputValueDefinition -> TypeDefinition
  *
  * Name -> DirectiveDefinition
  *
  * Name -> TypeDefinition
  *
  * Name -> FieldDefinition
  * FieldDefinition -> TypeDefinition
  *
  * Name -> FragmentSpread
  * Name -> FragmentDefinition
  * FragmentDefinition -> TypeDefinition
  * FragmentDefinition -> Set[Variable]
  *
  * Name -> OperationDefinition
  * OperationDefinition -> Set[Variable] (required variables needed to execute operation)
  *
  * Name -> ObjectTypeDefinition
  * ObjectTypeDefinition -> Set[InterfaceTypeDefinition]
  *
  * Name -> InterfaceTypeDefinition
  * InterfaceTypeDefinition -> Set[InterfaceTypeDefinition]
  * ```
  */

private def mapify[T <: HasName](ts: List[T]): Map[Name, T] = Map.from(ts.map(t => t.name -> t))

object SchemaContext:
    def apply(schema: TypeSystemDocument): SchemaContext =
        val definitions = schema.definitions ++ schema.builtInScalarDefs

        val schemaDefs    = definitions.collect { case o: SchemaDefinition => o }
        val typeDefs      = definitions.collect { case o: TypeDefinition => o }
        val directiveDefs = definitions.collect { case o: DirectiveDefinition => o }
        val operationDefs = definitions.collect { case o: OperationDefinition => o }

        new SchemaContext(
          schemaDefs.headOption,
          typeDefs,
          directiveDefs,
          operationDefs
        )

class SchemaContext(
    val schemaDef: Option[SchemaDefinition],
    val typeDefs: List[TypeDefinition],
    val directiveDefs: List[DirectiveDefinition],
    val operationDefs: List[OperationDefinition]
):
    private def mapifyFieldDefs(
        objLikeTypeDefs: List[ObjectLikeTypeDefinition]
    ): Map[NamedType, Map[Name, FieldDefinition]] =
        val scopedFieldDefs = scopeFieldDefs(objLikeTypeDefs) // group field defs into their scopes
        scopedFieldDefs.map((namedType, defs) => namedType -> mapify(defs))

    private def mapifyOpTypeDefs(schemaDef: Option[SchemaDefinition]): Map[OperationType, TypeDefinition] =
        schemaDef match
            case Some(SchemaDefinition(_, roots)) =>
                Map.from(
                  roots.toList.flatMap(rootOpTypeDef =>
                      typeDef(rootOpTypeDef.namedType.name).map(typeDef => rootOpTypeDef.operationType -> typeDef)
                  )
                )
            case None =>
                Map.from(
                  List(
                    Query        -> Name("Query"),
                    Mutation     -> Name("Mutation"),
                    Subscription -> Name("Subscription")
                  ).flatMap((opType, name) => typeDef(name).map(typeDef => opType -> typeDef))
                )

    private def scopeFieldDefs(
        objLikeTypeDefs: List[ObjectLikeTypeDefinition]
    ): Map[NamedType, List[FieldDefinition]] =
        @tailrec
        def recurse(
            objLikeTypeDefs: Set[ObjectLikeTypeDefinition],
            acc: List[(NamedType, List[FieldDefinition])] = Nil
        ): Map[NamedType, List[FieldDefinition]] =
            if objLikeTypeDefs.isEmpty then Map.from(acc)
            else
                val head    = objLikeTypeDefs.head
                val tail    = objLikeTypeDefs.tail
                val mapping = NamedType(head.name) -> head.fields
                val childObjLikeTypeDefs = head.fields
                    .map(_.`type`.name)
                    .flatMap(objLikeTypeDef)
                    .toSet
                recurse(childObjLikeTypeDefs union tail, mapping :: acc)
        end recurse

        recurse(objLikeTypeDefs.toSet)
    end scopeFieldDefs

    private def collectDirDefDeps(dirDef: DirectiveDefinition): Set[Name] =
        def toNameSet(dirs: List[Directive]): Set[Name] = dirs.map(_.name).toSet

        @tailrec
        def recurse(acc: List[HasDirectives], accDeps: Set[Name] = Set.empty): Set[Name] =
            acc match
                case Nil => accDeps
                case (ScalarTypeDefinition(_, dirs)) :: tail =>
                    recurse(tail, toNameSet(dirs) union accDeps)
                case (InputValueDefinition(_, tpe, _, dirs)) :: tail =>
                    val more = typeDef(tpe.name).toList
                    recurse(more ++ tail, toNameSet(dirs) union accDeps)
                case (FieldDefinition(_, args, tpe, dirs)) :: tail =>
                    val more = args ++ typeDef(tpe.name).toList
                    recurse(more ++ tail, toNameSet(dirs) union accDeps)
                case (ObjectTypeDefinition(_, ints, dirs, fields)) :: tail =>
                    val more = fields ++ ints.map(_.name).flatMap(typeDef)
                    recurse(more ++ tail, toNameSet(dirs) union accDeps)
                case (InterfaceTypeDefinition(_, ints, dirs, fields)) :: tail =>
                    val more = fields ++ ints.map(_.name).flatMap(typeDef)
                    recurse(more ++ tail, toNameSet(dirs) union accDeps)
                case (UnionTypeDefinition(_, dirs, tpes)) :: tail =>
                    val more = tpes.map(_.name).flatMap(typeDef)
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

        recurse(dirDef.arguments.map(_.`type`.name).flatMap(typeDef))
    end collectDirDefDeps

    private def collectInObjTypeDefDeps(typeDef: InputObjectTypeDefinition): Set[Name] =
        typeDef.fields
            .map(_.`type`.name)
            .flatMap(inObjTypeDef)
            .map(_.name)
            .toSet

    val objTypeDefs     = typeDefs.collect { case o: ObjectTypeDefinition => o }
    lazy val ifTypeDefs = typeDefs.collect { case o: InterfaceTypeDefinition => o }
    val objLikeTypeDefs = typeDefs.collect { case o: ObjectLikeTypeDefinition => o }
    val inObjTypeDefs   = typeDefs.collect { case o: InputObjectTypeDefinition => o }

    val typeDef        = mapify(typeDefs).get
    val objTypeDef     = mapify(objTypeDefs).get
    lazy val ifTypeDef = mapify(ifTypeDefs).get
    val objLikeTypeDef = mapify(objLikeTypeDefs).get
    val inObjTypeDef   = mapify(inObjTypeDefs).get
    val directiveDef   = mapify(directiveDefs).get
    val operationDef   = mapify(operationDefs).get
    val opTypeDef      = mapifyOpTypeDefs(schemaDef).get
    val fieldDef = mapifyFieldDefs(objLikeTypeDefs).get
        .andThen {
            case None            => (name: Name) => None
            case Some(fieldDefs) => fieldDefs.get
        }

    val dirDefDepGraph       = DepGraph.from(directiveDefs, collectDirDefDeps)
    val inObjTypeDefDepGraph = DepGraph.from(inObjTypeDefs, collectInObjTypeDefDeps)
end SchemaContext

class DocumentContext(
    val opDefs: List[OperationDefinition],
    val fragDefs: List[FragmentDefinition]
):
    private def collectFragDefDeps(fragDef: FragmentDefinition): Set[Name] =
        @tailrec
        def recurse(accSelectionSet: List[Selection], accDeps: Set[Name] = Set.empty): Set[Name] =
            accSelectionSet match
                case Nil                            => accDeps
                case (field: Field) :: tail         => recurse(field.selectionSet ::: tail, accDeps)
                case (frag: InlineFragment) :: tail => recurse(frag.selectionSet.toList ::: tail, accDeps)
                case (frag: FragmentSpread) :: tail => recurse(tail, accDeps + frag.name)

        recurse(fragDef.selectionSet.toList)
    end collectFragDefDeps

    private def collectOpDefDeps(fragDefDeps: DepGraph)(opDef: OperationDefinition): Set[Name] =
        @tailrec
        def recurse(accSelectionSets: List[Selection], accReqs: Set[Name] = Set.empty): Set[Name] =
            def requiredVars(args: List[Argument]): Set[Name] = args.flatMap {
                case Argument(_, Variable(name)) => Some(name)
                case _                           => None
            }.toSet

            accSelectionSets match
                case Nil => accReqs
                case (field: Field) :: tail =>
                    val allArgs = field.directives.flatMap(_.arguments) ++ field.arguments
                    val vars    = requiredVars(allArgs)
                    recurse(field.selectionSet ::: tail, vars union accReqs)
                case (frag: InlineFragment) :: tail =>
                    val vars = requiredVars(frag.directives.flatMap(_.arguments))
                    recurse(frag.selectionSet.toList ::: tail, vars union accReqs)
                case (frag: FragmentSpread) :: tail =>
                    val vars     = requiredVars(frag.directives.flatMap(_.arguments))
                    val fragDeps = fragDefDeps.deps.get(frag.name)
                    fragDeps match
                        case Some(deps) => recurse(tail, vars union deps union accReqs)
                        case None       =>
                            // we ignore missing dependencies at this stage
                            recurse(tail, vars union accReqs)
        end recurse

        recurse(opDef.selectionSet.toList)
    end collectOpDefDeps

    @tailrec
    private def collectFragSpreads(
        sels: List[Selection],
        accFragSpread: Set[FragmentSpread] = Set.empty
    ): Set[FragmentSpread] =
        sels match
            case Nil                                  => accFragSpread
            case Field(_, _, _, _, fieldSels) :: tail => collectFragSpreads(fieldSels ::: tail, accFragSpread)
            case InlineFragment(_, _, inFragSels) :: tail =>
                collectFragSpreads(inFragSels.toList ::: tail, accFragSpread)
            case (fragSpread: FragmentSpread) :: tail => collectFragSpreads(tail, accFragSpread + fragSpread)

    val fragSpreads = collectFragSpreads(???).toList

    val opDef   = mapify(opDefs).get
    val fragDef = mapify(fragDefs).get

    val fragDefDepGraph = DepGraph.from(fragDefs, collectFragDefDeps)
    val opDefDepGraph   = DepGraph.from(opDefs, collectOpDefDeps(fragDefDepGraph))
end DocumentContext

class Context(val schemaCtx: SchemaContext, val docCtx: DocumentContext):
    export docCtx.*
    export schemaCtx.*
    given SchemaContext = schemaCtx

    // private val opType2Name = (opType: OperationType) =>
    //     schemaDef match
    //         case Some(SchemaDefinition(_, roots)) =>
    //             roots
    //                 .find(_.operationType == opType)
    //                 .map(_.namedType.name)
    //         case None =>
    //             opType match
    //                 case Query        => Some(Name("Query"))
    //                 case Mutation     => Some(Name("Mutation"))
    //                 case Subscription => Some(Name("Subscription"))

    // val opTypeDef = (Kleisli(typeDef) compose Kleisli(opType2Name)).run

// case class VariableSymbolTable(
//     m_name_def: Map[Name, VariableDefinition],
//     m_def_typeDef: Map[VariableDefinition, TypeDefinition]
// ):
//     val name_def     = Kleisli(m_name_def.get)
//     val def_typeDef  = Kleisli(m_def_typeDef.get)
//     val name_typeDef = def_typeDef compose name_def

// case class ArgumentSymbolTable(
//     m_name_def: Map[Name, InputValueDefinition],
//     m_def_typeDef: Map[InputValueDefinition, TypeDefinition]
// ):
//     val name_def     = Kleisli(m_name_def.get)
//     val def_typeDef  = Kleisli(m_def_typeDef.get)
//     val name_typeDef = def_typeDef compose name_def

// case class DirectiveSymbolTable(m_name_def: Map[Name, DirectiveDefinition]):
//     val name_def = Kleisli(m_name_def.get)

// case class TypeSymbolTable(m_name_def: Map[Name, TypeDefinition]):
//     val name_def = Kleisli(m_name_def.get)

// case class FieldSymbolTable(
//     m_name_def: Map[Name, FieldDefinition],
//     m_def_typeDef: Map[FieldDefinition, TypeDefinition]
// ):
//     val name_def     = Kleisli(m_name_def.get)
//     val def_typeDef  = Kleisli(m_def_typeDef.get)
//     val name_typeDef = def_typeDef compose name_def

// case class FragmentSymbolTable(
//     m_name_spread: Map[Name, FragmentSpread],
//     m_name_def: Map[Name, FragmentDefinition],
//     m_def_typeDef: Map[FragmentDefinition, TypeDefinition],
//     depGraph: DepGraph
// ):
//     val name_spread  = Kleisli(m_name_spread.get)
//     val name_def     = Kleisli(m_name_def.get)
//     val def_typeDef  = Kleisli(m_def_typeDef.get)
//     val def_deps     = Kleisli((fragDef: FragmentDefinition) => depGraph.deps.get(fragDef.name))
//     val name_typeDef = def_typeDef compose name_def
//     val name_deps    = def_deps compose name_def

// case class OperationSymbolTable(m_name_def: Map[Name, OperationDefinition]):
//     val opDefs   = m_name_def.values.toList
//     val name_def = Kleisli(m_name_def.get)

// case class OperationDependencySymbolTable(opSymTab: OperationSymbolTable, depGraph: DepGraph):
//     val def_deps  = Kleisli((opDef: OperationDefinition) => depGraph.deps.get(opDef.name))
//     val name_deps = def_deps compose opSymTab.name_def

// case class ObjectSymbolTable(
//     m_name_def: Map[Name, ObjectTypeDefinition],
//     m_def_extends: Map[ObjectTypeDefinition, Set[InterfaceTypeDefinition]]
// ):
//     val name_def     = Kleisli(m_name_def.get)
//     val def_extends  = Kleisli(m_def_extends.get)
//     val name_extends = def_extends compose name_def

// case class InterfaceSymbolTable(
//     m_name_def: Map[Name, InterfaceTypeDefinition],
//     m_def_extends: Map[InterfaceTypeDefinition, Set[InterfaceTypeDefinition]]
// ):
//     val name_def     = Kleisli(m_name_def.get)
//     val def_extends  = Kleisli(m_def_extends.get)
//     val name_extends = def_extends compose name_def

// /** A symbol table for entities which exist only in the schema.
//   *
//   * This symbol table need only be constructed once, when the schema is passed to the server.
//   *
//   * @param opSymTab
//   * @param objSymTab
//   * @param ifSymTab
//   */
// case class SchemaSymbolTable(
//     typeSymTab: TypeSymbolTable,
//     opSymTab: OperationSymbolTable,
//     objSymTab: ObjectSymbolTable,
//     ifSymTab: InterfaceSymbolTable
// )

// /** A symbol table for entities which exist in the document. Note that these entities may point to entities in the
//   * SchemaSymbolTable [[SchemaSymbolTable]].
//   *
//   * This symbol table needs to be created for each document request made to the server. It can be cached for future
//   * requests.
//   *
//   * @param fragSymTab
//   */
// case class DocSymbolTable(
//     fieldSymTab: FieldSymbolTable,
//     fragSymTab: FragmentSymbolTable,
//     opDepSymTab: OperationDependencySymbolTable
// )

// // case class SymbolTable(
// //     varSymTab: VariableSymbolTable,
// //     argSymTab: ArgumentSymbolTable,
// //     dirSymTab: DirectiveSymbolTable,
// //     typeSymTab: TypeSymbolTable,
// //     fieldSymTab: FieldSymbolTable
// // )

// // ====================================================================================================================
// // Funtions for build dependency graphs
// // ====================================================================================================================

// @tailrec
// private def collectFragDefDeps(accSelectionSet: List[Selection], accDeps: Set[Name] = Set.empty): Set[Name] =
//     accSelectionSet match
//         case Nil                            => accDeps
//         case (field: Field) :: tail         => collectFragDefDeps(field.selectionSet ::: tail, accDeps)
//         case (frag: InlineFragment) :: tail => collectFragDefDeps(frag.selectionSet.toList ::: tail, accDeps)
//         case (frag: FragmentSpread) :: tail => collectFragDefDeps(tail, accDeps + frag.name)
// end collectFragDefDeps

// @tailrec
// def collectOpDefDeps(
//     accSelectionSets: List[Selection],
//     fragDefDeps: DepGraph,
//     accReqs: Set[Name] = Set.empty
// ): Set[Name] =
//     def requiredVars(args: List[Argument]): Set[Name] = args.flatMap {
//         case Argument(_, Variable(name)) => Some(name)
//         case _                           => None
//     }.toSet

//     accSelectionSets match
//         case Nil => accReqs
//         case (field: Field) :: tail =>
//             val allArgs = field.directives.flatMap(_.arguments) ++ field.arguments
//             val vars    = requiredVars(allArgs)
//             collectOpDefDeps(field.selectionSet ::: tail, fragDefDeps, vars union accReqs)
//         case (frag: InlineFragment) :: tail =>
//             val vars = requiredVars(frag.directives.flatMap(_.arguments))
//             collectOpDefDeps(frag.selectionSet.toList ::: tail, fragDefDeps, vars union accReqs)
//         case (frag: FragmentSpread) :: tail =>
//             val vars     = requiredVars(frag.directives.flatMap(_.arguments))
//             val fragDeps = fragDefDeps.deps.get(frag.name)
//             fragDeps match
//                 case Some(deps) => collectOpDefDeps(tail, fragDefDeps, vars union deps union accReqs)
//                 case None       =>
//                     // we ignore missing dependencies at this stage
//                     collectOpDefDeps(tail, fragDefDeps, vars union accReqs)
// end collectOpDefDeps

// private def collectDeps(fragDef: FragmentDefinition): Set[Name] = collectFragDefDeps(fragDef.selectionSet.toList)

// private def collectDeps(fragDefDeps: DepGraph)(opDef: OperationDefinition): Set[Name] =
//     collectOpDefDeps(opDef.selectionSet.toList, fragDefDeps)

// // ====================================================================================================================
// // Functions for building symbol tables
// // ====================================================================================================================

// def buildTypeSymTab(typeDefs: List[TypeDefinition]): TypeSymbolTable =
//     val m_name_typeDef = Map.from(typeDefs.map(typeDef => typeDef.name -> typeDef))
//     TypeSymbolTable(m_name_typeDef)

// def buildVarSymTab(opDefs: List[OperationDefinition], typeSymTab: TypeSymbolTable): VariableSymbolTable =
//     val varDefs    = opDefs.flatMap(opDef => opDef.variableDefinitions)
//     val m_name_def = Map.from(varDefs.map(varDef => varDef.name -> varDef))
//     val m_def_typeDef = Map.from(
//       varDefs.flatMap(varDef => typeSymTab.name_def(varDef.name).map(typeDef => varDef -> typeDef))
//     )
//     VariableSymbolTable(m_name_def, m_def_typeDef)

// def buildFieldSymTab(fieldDefs: List[FieldDefinition], schemaSymTab: SchemaSymbolTable): FieldSymbolTable =
//     val m_name_def = Map.from(fieldDefs.map(fieldDef => fieldDef.name -> fieldDef))
//     val m_def_typeDef = Map.from(
//       fieldDefs.flatMap(fieldDef =>
//           schemaSymTab.typeSymTab
//               .name_def(fieldDef.`type`.name)
//               .map(typeDef => fieldDef -> typeDef)
//       )
//     )
//     FieldSymbolTable(m_name_def, m_def_typeDef)

// def buildFragSymTab(
//     fragSpreads: List[FragmentSpread],
//     fragDefs: List[FragmentDefinition],
//     schemaSymTab: SchemaSymbolTable
// ): FragmentSymbolTable =
//     val m_name_spread = Map.from(fragSpreads.map(fragSpread => fragSpread.name -> fragSpread))
//     val m_name_def    = Map.from(fragDefs.map(fragDef => fragDef.name -> fragDef))
//     val m_def_typeDef =
//         Map.from(
//           fragDefs.flatMap(fragDef =>
//               schemaSymTab.typeSymTab
//                   .name_def(fragDef.on.name)
//                   .map(typeDef => fragDef -> typeDef)
//           )
//         )

//     val depGraph = DepGraph.from(fragDefs, collectDeps)

//     FragmentSymbolTable(m_name_spread, m_name_def, m_def_typeDef, depGraph)

// def buildOpSymTab(opDefs: List[OperationDefinition]): OperationSymbolTable =
//     val m_name_def = Map.from(opDefs.map(opDef => opDef.name -> opDef))
//     OperationSymbolTable(m_name_def)

// def buildOpDepSymTab(opSymTab: OperationSymbolTable, fragDefDepGraph: DepGraph): OperationDependencySymbolTable =
//     val opDefDepGraph = DepGraph.from(opSymTab.opDefs, collectDeps(fragDefDepGraph))
//     OperationDependencySymbolTable(opSymTab, opDefDepGraph)

// def buildObjSymTab(objDefs: List[ObjectTypeDefinition], ifSymTab: InterfaceSymbolTable): ObjectSymbolTable =
//     val m_name_def = Map.from(objDefs.map(o => o.name -> o))
//     val m_def_extends = Map.from(objDefs.map(o =>
//         val extended = o.interfaces.flatMap(e => ifSymTab.name_def(e.name)).toSet
//         o -> extended
//     ))
//     ObjectSymbolTable(m_name_def, m_def_extends)

// def buildIfSymTab(ifDefs: List[InterfaceTypeDefinition]): InterfaceSymbolTable =
//     val m_name_def = Map.from(ifDefs.map(i => i.name -> i))
//     val m_def_extends = Map.from(ifDefs.map(i =>
//         val extended = i.interfaces.flatMap(e => m_name_def.get(e.name)).toSet
//         i -> extended
//     ))
//     InterfaceSymbolTable(m_name_def, m_def_extends)

// def buildSchemaSymTab(schema: TypeSystemDocument): SchemaSymbolTable =
//     val typeDefs = schema.definitions.collect { case o: TypeDefinition => o }
//     val opDefs   = schema.definitions.collect { case o: OperationDefinition => o }
//     val objDefs  = schema.definitions.collect { case o: ObjectTypeDefinition => o }
//     val ifDefs   = schema.definitions.collect { case o: InterfaceTypeDefinition => o }

//     val typeSymTab = buildTypeSymTab(typeDefs)
//     val opSymTab   = buildOpSymTab(opDefs)
//     val ifSymTab   = buildIfSymTab(ifDefs)
//     val objSymTab  = buildObjSymTab(objDefs, ifSymTab)

//     SchemaSymbolTable(typeSymTab, opSymTab, objSymTab, ifSymTab)

// def buildDocSymTab(doc: ExecutableDocument, schemaSymTab: SchemaSymbolTable): DocSymbolTable =
//     @tailrec
//     def collectFragSpreads(sels: List[Selection], accFragSpread: Set[FragmentSpread] = Set.empty): Set[FragmentSpread] =
//         sels match
//             case Nil                                  => accFragSpread
//             case Field(_, _, _, _, fieldSels) :: tail => collectFragSpreads(fieldSels ::: tail, accFragSpread)
//             case InlineFragment(_, _, inFragSels) :: tail =>
//                 collectFragSpreads(inFragSels.toList ::: tail, accFragSpread)
//             case (fragSpread: FragmentSpread) :: tail => collectFragSpreads(tail, accFragSpread + fragSpread)

//     val fragSpreads = doc.definitions.map(d => collectFragSpreads(d.selectionSet.toList)).reduce(_ union _).toList
//     val fragDefs    = doc.definitions.collect { case o: FragmentDefinition => o }

//     val fragDefDepGraph = DepGraph.from(fragDefs, collectDeps)

//     // val fieldSymTab = buildF
//     val fragSymTab  = buildFragSymTab(fragSpreads, fragDefs, schemaSymTab)
//     val opDepSymTab = buildOpDepSymTab(schemaSymTab.opSymTab, fragDefDepGraph)

//     // DocSymbolTable()
//     ???

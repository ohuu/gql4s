package gql4s
package validation

import scala.annotation.tailrec
import scala.collection.mutable.LinkedHashMap

import cats.data.NonEmptyList

import munit.FunSuite
import parsing.*
import parsing.Type.NamedType

class DepGraphSuite extends FunSuite:
    // Note: these frag defs are NOT legal graphql

    val hasCycles1 = List(
        FragmentDefinition(Name("A"), NamedType(Name("T")), Nil, List(FragmentSpread(Name("B"), Nil))),
        FragmentDefinition(Name("B"), NamedType(Name("T")), Nil, List(FragmentSpread(Name("A"), Nil)))
    )

    val hasCycles2 = List(
        FragmentDefinition(Name("E"), NamedType(Name("T")), Nil, List(FragmentSpread(Name("D"), Nil))),
        FragmentDefinition(Name("D"), NamedType(Name("T")), Nil, List(FragmentSpread(Name("B"), Nil))),
        FragmentDefinition(Name("C"), NamedType(Name("T")), Nil, List(FragmentSpread(Name("A"), Nil))),
        FragmentDefinition(Name("B"), NamedType(Name("T")), Nil, List(FragmentSpread(Name("C"), Nil))),
        FragmentDefinition(Name("A"), NamedType(Name("T")), Nil, List(FragmentSpread(Name("D"), Nil)))
    )

    val noCycles1 = List(
        FragmentDefinition(Name("D"), NamedType(Name("T")), Nil, List(FragmentSpread(Name("B"), Nil))),
        FragmentDefinition(Name("C"), NamedType(Name("T")), Nil, List(FragmentSpread(Name("A"), Nil))),
        FragmentDefinition(Name("B"), NamedType(Name("T")), Nil, List(FragmentSpread(Name("C"), Nil))),
        FragmentDefinition(Name("A"), NamedType(Name("T")), Nil, List(Field(None, Name("a"), Nil, Nil, Nil)))
    )

    val noCycles2 = List(
        FragmentDefinition(Name("D"), NamedType(Name("T")), Nil, List(FragmentSpread(Name("B"), Nil))),
        FragmentDefinition(Name("C"), NamedType(Name("T")), Nil, List(FragmentSpread(Name("A"), Nil))),
        FragmentDefinition(Name("B"), NamedType(Name("T")), Nil, List(FragmentSpread(Name("C"), Nil))),
        FragmentDefinition(Name("B"), NamedType(Name("T")), Nil, List(FragmentSpread(Name("E"), Nil))),
        FragmentDefinition(Name("E"), NamedType(Name("T")), Nil, List(FragmentSpread(Name("A"), Nil))),
        FragmentDefinition(Name("A"), NamedType(Name("T")), Nil, List(Field(None, Name("a"), Nil, Nil, Nil)))
    )

    @tailrec
    private def collectDeps(accSelectionSet: List[Selection], accDeps: Set[Name] = Set.empty): Set[Name] =
        accSelectionSet match
            case Nil                            => accDeps
            case (field: Field) :: tail         => collectDeps(field.selectionSet ::: tail, accDeps)
            case (frag: InlineFragment) :: tail => collectDeps(frag.selectionSet.toList ::: tail, accDeps)
            case (frag: FragmentSpread) :: tail => collectDeps(tail, accDeps + frag.name)
        end match
    end collectDeps

    def collectDeps(fragDef: FragmentDefinition): Set[Name] = collectDeps(fragDef.selectionSet.toList)

    val graph1 = DepGraph.from(hasCycles1, collectDeps)
    val graph2 = DepGraph.from(hasCycles2, collectDeps)
    val graph3 = DepGraph.from(noCycles1, collectDeps)
    val graph4 = DepGraph.from(noCycles2, collectDeps)

    test("graph should work!") {
        assertEquals(
            clue(graph1.deps),
            clue(LinkedHashMap(Name("A") -> Set(Name("B")), Name("B") -> Set(Name("A"))))
        )
        assertEquals(
            clue(graph1.topo),
            clue(Topo.HasCycles(List(Name("A"), Name("B"))))
        )

        assertEquals(
            clue(graph2.deps),
            clue(
                LinkedHashMap(
                    Name("E") -> Set(Name("D")),
                    Name("D") -> Set(Name("B")),
                    Name("C") -> Set(Name("A")),
                    Name("B") -> Set(Name("C")),
                    Name("A") -> Set(Name("D"))
                )
            )
        )
        assertEquals(
            clue(graph2.topo),
            clue(Topo.HasCycles(List(Name("D"), Name("C"), Name("B"), Name("A"))))
        )

        assertEquals(
            clue(graph3.deps),
            clue(
                LinkedHashMap(
                    Name("D") -> Set(Name("B")),
                    Name("C") -> Set(Name("A")),
                    Name("B") -> Set(Name("C")),
                    Name("A") -> Set.empty
                )
            )
        )
        assertEquals(
            clue(graph3.topo),
            clue(Topo.NoCycles(List(Name("D"), Name("B"), Name("C"), Name("A"))))
        )

        assertEquals(
            clue(graph4.deps),
            clue(
                LinkedHashMap(
                    Name("D") -> Set(Name("B")),
                    Name("C") -> Set(Name("A")),
                    Name("B") -> Set(Name("C"), Name("E")),
                    Name("E") -> Set(Name("A")),
                    Name("A") -> Set.empty
                )
            )
        )
        assertEquals(
            clue(graph4.topo),
            clue(Topo.NoCycles(List(Name("D"), Name("B"), Name("C"), Name("E"), Name("A"))))
        )
    }

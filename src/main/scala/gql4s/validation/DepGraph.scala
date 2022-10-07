package gql4s
package validation

import scala.annotation.tailrec
import scala.collection.mutable.LinkedHashMap

import cats.kernel.Semigroup

import parsing.HasName
import parsing.Name

enum Topo:
    case NoCycles(order: List[Name])
    case HasCycles(cycles: List[Name])

object DepGraph:
    def from[T <: HasName](ts: List[T], depsFn: T => Set[Name]): DepGraph =
        DepGraph(
            ts.map(t => t.name -> depsFn(t))
                .foldLeft(LinkedHashMap.empty)((acc, next) =>
                    val (name, deps) = next
                    acc.addOne(name -> (acc.getOrElse(name, Set.empty) union deps))
                )
        )

case class DepGraph(deps: LinkedHashMap[Name, Set[Name]]):
    @tailrec
    private def topoSort(
        deps: LinkedHashMap[Name, Set[Name]],
        ordering: LinkedHashMap[Name, Set[Name]] = LinkedHashMap.empty
    ): Topo =
        if deps.isEmpty then Topo.NoCycles(ordering.keys.toList)
        else
            val zeroDegree = deps.find((name, _) => deps.find((_, deps) => deps.contains(name)).isEmpty)

            zeroDegree match
                case None => Topo.HasCycles(deps.keys.toList)
                case Some(zeroDegree @ (zeroDegreeFragDef, _)) =>
                    val filteredMap = deps.filter((fragDef, _) => fragDef != zeroDegreeFragDef)
                    topoSort(filteredMap, ordering += zeroDegree)

    lazy val topo: Topo = topoSort(deps)
end DepGraph

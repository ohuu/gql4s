// Copyright (c) 2021 by Oliver Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

package gql4s
package validation

import cats.implicits.*
import scala.annotation.tailrec
import scala.collection.mutable.LinkedHashMap
import gql4s.GqlError
import GqlError.*

type DependencyGraph[K <: HasName] = LinkedHashMap[K, Set[Name]]

def topologicalSort[K <: HasName](
    graph: DependencyGraph[K]
): Either[List[GqlError], DependencyGraph[K]] =
  @tailrec
  def recurse(
      graph: DependencyGraph[K],
      ordering: DependencyGraph[K] = LinkedHashMap.empty
  ): Either[List[GqlError], DependencyGraph[K]] =
    if graph.isEmpty then ordering.asRight
    else
      // find a node with zero inputs
      val zeroDegree = graph.find((k, _) => graph.find((_, v) => v.contains(k.name)).isEmpty)

      zeroDegree match
        // 5.5.2.2 Fragment definitions must not contain cycles
        case None =>
          graph.map((k, _) => ContainsCycles(k.name)).toList.asLeft[DependencyGraph[K]]
        case Some(zeroDegree @ (zeroDegreeFragDef, _)) =>
          recurse(
            graph.filter((fragDef, _) => fragDef != zeroDegreeFragDef),
            ordering += zeroDegree
          )
  end recurse

  recurse(graph)
end topologicalSort

def containsCycles[K <: HasName](graph: DependencyGraph[K]): List[GqlError] =
  topologicalSort(graph) match
    case Right(_)   => Nil
    case Left(errs) => errs

// Copyright (c) 2021 by Oliver Winks
// This software is licensed under the MIT License (MIT).
// For more information see LICENSE or https://opensource.org/licenses/MIT

// package gql4s
// package validation

// import scala.annotation.tailrec
// import scala.collection.mutable
// import scala.collection.mutable.LinkedHashMap

// import cats.data.ValidatedNec
// import cats.implicits.*
// import cats.kernel.Semigroup

// import errors.*
// import errors.GqlError.*
// import parsing.*

// type DependencyGraph[K] = LinkedHashMap[K, Set[Name]]

// given [K <: HasName]: Semigroup[DependencyGraph[K]] with
//   def combine(x: DependencyGraph[K], y: DependencyGraph[K]) = x ++ y

// /** @return
//   *   either a copy of the dependency graph topologically sorted, or a list of nodes which contain cycles
//   */
// def topologicalSort[K <: HasName](
//     graph: DependencyGraph[K]
// ): Validated[DependencyGraph[K]] =
//   @tailrec
//   def recurse(
//       graph: DependencyGraph[K],
//       ordering: DependencyGraph[K] = LinkedHashMap.empty
//   ): Validated[DependencyGraph[K]] =
//     if graph.isEmpty then ordering.validNec
//     else
//       // find a node with zero inputs
//       val zeroDegree = graph.find((k, _) => graph.find((_, v) => v.contains(k.name)).isEmpty)

//       zeroDegree match
//         // 5.5.2.2 Fragment definitions must not contain cycles
//         case None =>
//           graph.keys.toList
//             .map(k => CycleDetected(k.name).invalidNec[DependencyGraph[K]])
//             .reduceLeft(_ combine _) // k has cycles!
//         case Some(zeroDegree @ (zeroDegreeFragDef, _)) =>
//           recurse(
//             graph.filter((fragDef, _) => fragDef != zeroDegreeFragDef),
//             ordering += zeroDegree
//           )
//   end recurse

//   recurse(graph)
// end topologicalSort

// def containsCycles[K <: HasName](graph: DependencyGraph[K]): Validated[DependencyGraph[K]] =
//   topologicalSort(graph)

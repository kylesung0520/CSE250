/**
 * cse250.pa3.tests.MapUtilityTests.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 */
package cse250.pa3.tests
  import org.scalatest.{BeforeAndAfter, FlatSpec}
  import cse250.pa3.MapUtilities
  import cse250.objects.{StreetGraph, TaxParcel}
  import scala.collection.mutable

  class MapUtilityTests extends FlatSpec with BeforeAndAfter {
   // Your tests for problem 1 should be contained under this header.
    behavior of "MapUtilityTests 1(a)"
    it should "edge list should be empty if all vertices disconnected" in {
      val loadId = mutable.Set[String]()
      loadId += "1"

      val loadM = mutable.Map[String, mutable.Set[String]]()
      loadM += ("4" -> mutable.Set[String]("a,b"))

      val graph = MapUtilities.buildIntersectionGraph(loadId, loadM)
      assert(graph.edges.size == 0)
      assert(graph.edges.isEmpty)
    }


    it should "num of edges should be {(num of connected vertices-1)*2}" in {
      val loadI = mutable.Set[String]()
      loadI += "1"
      loadI += "2"
      val loadM = mutable.Map[String, mutable.Set[String]]()
      loadM += ("1" -> mutable.Set[String]("a,b"))
      loadM += ("2" -> mutable.Set[String]("b,c"))
      val graph = MapUtilities.buildIntersectionGraph(loadI, loadM)
      assert(graph.edges.size == 4)
      assert(graph.edges.contains("A", "B"))
      assert(graph.edges.contains("B", "A"))
      assert(graph.edges.contains("B", "C"))
      assert(graph.edges.contains("C", "B"))
    }


    behavior of "MapUtilityTests 1(b)"
    it should "same street return 0" in {
      val graph = new StreetGraph
      val start = new TaxParcel
      val end = new TaxParcel
      val a = MapUtilities.computeFewestTurns(graph, start, end)
      if (start.parcelInfo("STREET") == end.parcelInfo("STREET")) {
       assert(a == 0)
      }
    }

    it should "not possible to reach the other property return -1" in {
      val graph = new StreetGraph
      val start = new TaxParcel
      val end = new TaxParcel
      graph.insertVertex("A")
      graph.insertVertex("B")
      graph.insertVertex("C")
      val a = MapUtilities.computeFewestTurns(graph, start, end)
      if (graph.edges.size != ((graph.vertices.size - 1) * 2)) {
        assert(a == -1)
      }
    }
    it should "return the number of turn" in {
      val graph = new StreetGraph
      val start = new TaxParcel
      val end = new TaxParcel
      graph.insertVertex("A")
      graph.insertVertex("B")
      graph.insertVertex("C")
      graph.insertEdge("A", "B")
      graph.insertEdge("B", "A")
      graph.insertEdge("B", "C")
      graph.insertEdge("C", "B")
      graph.insertEdge("C", "D")
      graph.insertEdge("D", "C")
      val a = MapUtilities.computeFewestTurns(graph, start, end)
      if (graph.edges == (graph.vertices.size - 1) * 2) {
        assert(a == graph.vertices.size - 1)
      }
    }

    // ^^^
    behavior of "Other Functionality"
    it should "..."

}


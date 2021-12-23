/**
 * cse250.pa3.Main.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Modify at your leisure, but this will not be graded.
 */
package cse250.pa3

import cse250.objects.{StreetGraph, AssessmentUtilities}

object Main {
  def main(args: Array[String]): Unit = {
    val taxAssessmentFilename = "data/small_set.csv"
    val entries = AssessmentUtilities.loadAssessmentEntries(taxAssessmentFilename, 25)
    val mapXMLFile = "data/buffalo-map.xml"
    val intersectionNodeXMLFile = "data/export.osm"
    val intersectionIDs = MapUtilities.loadIntersectionIDs(intersectionNodeXMLFile)
    val nodeToStreetMapping = MapUtilities.loadMapInfo(mapXMLFile)
    val streetGraph = MapUtilities.buildIntersectionGraph(intersectionIDs, nodeToStreetMapping)
    println(streetGraph.vertices.size)
    println(streetGraph.edges.size)
/*
    println(s"${entries(1).parcelInfo("STREET")} to\n${entries(2).parcelInfo("STREET")}")
    println(MapUtilities.computeFewestTurns(streetGraph, entries(1), entries(2)))
    println(MapUtilities.computeFewestTurnsList(streetGraph, entries(1), entries(2)))

    println(s"${entries(3).parcelInfo("STREET")} to\n${entries(24).parcelInfo("STREET")}")
    println(MapUtilities.computeFewestTurns(streetGraph, entries(3), entries(24)))
    println(MapUtilities.computeFewestTurnsList(streetGraph, entries(3), entries(24)))
  */
    //println(MapUtilities.computeFewestTurns(streetGraph,"WALTER", "WESCOTT"))
    //println(MapUtilities.computeFewestTurnsList(streetGraph,"WALTER", "ORLANDO"))
    //println(MapUtilities.computeFewestTurns(streetGraph,"WALTER", "ORLANDO"))
   // println(MapUtilities.computeFewestTurnsList(streetGraph,"WALTER", "ORLANDO"))


  }

}


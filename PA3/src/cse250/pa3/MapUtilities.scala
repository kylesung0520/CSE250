/**
 * cse250.pa3.MapUtilities.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 */
package cse250.pa3

import cse250.objects.{StreetGraph, TaxParcel}

import scala.collection.mutable
import scala.xml.XML

object MapUtilities {
  def loadIntersectionIDs(filename: String): mutable.Set[String] = {
    val xml = XML.loadFile(filename)
    val x = mutable.Set[String]()
    xml.child.foreach(y => if(y.label == "node") x += y.attributes("id").toString())
    x
  }

  def loadMapInfo(filename: String): mutable.Map[String, mutable.Set[String]] = {
    val xml = XML.loadFile(filename)
    val retVal = mutable.Map[String, mutable.Set[String]]()

    for(i <- xml.child){
      if(i.label == "way"){
        for(j <- i.child){
          if(j.label == "nd"){
            val v = j.attributes("ref").toString()
            for(k <- i.child){
              var a = mutable.Set[String]()
              if(k.label == "tag" && k.attributes("k").toString() == "tiger:name_base"){
                var q = k.attributes("v").toString()
                if(!retVal.contains(j.attributes("ref").toString())){
                  a += q
                  retVal += v -> a
                }
                else{
                  retVal.foreach(x => if(x._1 == v)x._2 += q)
                }
              }
            }
          }
        }
      }
    }
    retVal
  }

  def buildIntersectionGraph(intersectionIDs: mutable.Set[String],
                             nodeToStreetMapping: mutable.Map[String, mutable.Set[String]]): StreetGraph = {
    val streetGraph = new StreetGraph

    nodeToStreetMapping.foreach(x => intersectionIDs.foreach(y => if(x != "" && x == y) streetGraph.insertVertex(x._1.toUpperCase)))

    nodeToStreetMapping.foreach(x => intersectionIDs.foreach(y => if(x._1 != "" && x._1 == y){ if(x._2.toList.length >= 2) for(i <- 0 until x._2.toList.length-1){ for(j <- i+1 until x._2.toList.length) streetGraph.insertEdge(x._2.toList(i).toUpperCase(), x._2.toList(j).toUpperCase())}}))
    nodeToStreetMapping.foreach(x => intersectionIDs.foreach(y => if(x._1 != "" && x._1 == y){ if(x._2.toList.length >= 2) for(i <- x._2.toList.length-1 to 1 by -1){ for(j <- i-1 to 0 by - 1) streetGraph.insertEdge(x._2.toList(i).toUpperCase, x._2.toList(j).toUpperCase())}}))

    streetGraph
  }

  def computeFewestTurns(streetGraph: StreetGraph, start: TaxParcel, end: TaxParcel): Int = {
    var cnt = -1
    if(start.parcelInfo("STREET") == end.parcelInfo("STREET")){
      0
    }
    else if(streetGraph.edges.contains(start.parcelInfo("STREET"),end.parcelInfo("STREET"))){
      1
    }
    else{
      /*streetGraph.edges.foreach(x => if(x._1 == start){streetGraph.edges.foreach(y => if(y._2 == end){if(x._2 == y._1){cnt = 2} else if(streetGraph.edges.contains(x._2, y._1)){cnt = 3}})})*/
      cnt
    }
  }

  def computeFewestTurnsList(streetGraph: StreetGraph, start: TaxParcel, end: TaxParcel): Seq[String] = {
    /*if(start.parcelInfo("STREET") == end.parcelInfo("STREET")){
      List(start.parcelInfo("STREET"))
    }
    else{
      List()
    }
    var a = ""
    if(start == end){
      List()
    }
    else if(streetGraph.edges.contains(start, end)){
      a = start
    }
    else{
      streetGraph.edges.foreach(x => if(x._1 == start){
        streetGraph.edges.foreach(y => if(y._2 == end){
          if(x._2 == y._1){a = (x,y)}//cnt = 2}
          else if(streetGraph.edges.contains(x._2, y._1)){a :+ (x,(x._2,y._1,y))/*cnt = 3*/}
        })})
      a
    }*/
    if(start.parcelInfo("STREET") == end.parcelInfo("STREET")){
      List(start.parcelInfo("STREET"))
    }
    else if(streetGraph.edges.contains(start.parcelInfo("STREET"),end.parcelInfo("STREET"))){
      List(start.parcelInfo("STREET"),end.parcelInfo("STREET"))
    }
    else{
      List()
    }
  }
}

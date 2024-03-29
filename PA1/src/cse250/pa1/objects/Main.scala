/**
 * cse250.pa1.objects.Main.scala
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
package cse250.pa1.objects

import cse250.objects.{AssessmentUtilities, TaxParcel}

object Main {
  def main(args: Array[String]): Unit = {
    val taxParcelStore = new GroupByStore
    println(taxParcelStore.iterator.hasNext)
    println(taxParcelStore.iterator("").hasNext)
    val numLines = 25 min AssessmentUtilities.DATA_ROWS
    for (entry <- AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.DATA_FILENAME, AssessmentUtilities.DATA_ROWS)) {
      taxParcelStore.insert(entry)
    }
    println(s"Storage after $numLines additions:")
    println("-----")
    for(i <- taxParcelStore._groupings){
      println(i._value.parcelInfo(taxParcelStore._groupingAttribute))
    }
    println("length = " + taxParcelStore._groupings.length)
    println("numStored = " + taxParcelStore._numStored)
    var cnt = 1
    for(i <- 0 until taxParcelStore._groupings.length){
      var a = taxParcelStore._groupings(i)
      print(a._value.parcelInfo(taxParcelStore._groupingAttribute) + " ")
      while(a._next != null){
        cnt += 1
        print(a._value.parcelInfo(taxParcelStore._groupingAttribute) + " ")
        a = a._next
      }
      println()
    }
    println(cnt)
    println("----")
    taxParcelStore.regroup("NEIGHBORHOOD")
    for(i <- 0 until taxParcelStore._groupings.length){
      var cnt2 = 1
      var a = taxParcelStore._groupings(i)
      print(a._value.parcelInfo(taxParcelStore._groupingAttribute) + " ")
      while(a._next != null){
        cnt2 += 1
        print(a._value.parcelInfo(taxParcelStore._groupingAttribute) + " ")
        a = a._next
      }
      println()
      println(cnt2)
    }
    println("numStored = " + taxParcelStore._numStored)
    /*
    println(s"Storage after regrouping by NEIGHBORHOOD:")
    println("-----")
    println(taxParcelStore)
    println("-----")*/
  }
}

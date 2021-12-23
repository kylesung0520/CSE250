/**
 * cse250.pa0.objects.AssessmentDataProcessor.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 */
package cse250.pa0.objects

import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter

import cse250.objects.TaxParcel

import scala.io.Source

object AssessmentDataProcessor {
  def sanitizeData(filename: String): Unit = {
    // For opening files, look at Scala Cookbook File I/O Excerpt
    val inputFile = scala.io.Source.fromFile(filename)
    // Note: lines is an iterator to the file. This is only valid as long as the file is open.
    //       Ensure you do not close the file prior to finishing the file usage.
    val lines = inputFile.getLines()
    val outputFile = new BufferedWriter(new FileWriter( new File(filename + "-updated")))
    // Without the '\n' character, all output will be written as one long line.
    // Process the lines.
    val removed = Array(10,11,12,13,14,17,21,22,23,24,25,30,31,32,33,34,35,36,37,38,41,44,45,46)
    val header = lines.next().split(",")
    for(i <- 0 until header.length){
      if(!removed.contains(i)){
        outputFile.write(header(i))
        if(i != header.length-1){
          outputFile.write(',')
        }
      }
    }
    outputFile.newLine()
    while(lines.hasNext){
      val tmp = new StringBuilder(lines.next())
      var sw = 0; var meet = 0; var met = 0
      for(i <- 0 until tmp.length()){
        if(tmp(i) == '\"'){
          meet += 1
          if(sw != 0 && meet == met*2){
            sw = 0; meet = 0; met = 0
          }
          else if(sw == 0){
            sw = 1
          }
          else if(met == 0 && sw != 0){
            sw = 0
          }
        }
        else if(tmp(i) == ',' && sw == 0){
          tmp.update(i , '^')
          if(meet != 0){
            meet = 0
          }
        }
        else if(tmp(i) == ',' && sw == 1){
          met = meet
        }
      }
      val values = tmp.toString().split('^')
      for(i <- 0 until values.length){
        if(removed.contains(i) == false){
          outputFile.write(values(i))
          if(i != values.length-1){
            outputFile.write(',')
          }
        }
      }
      outputFile.newLine()
    }
    // Close the files at the end.
    inputFile.close()
    outputFile.close()
  }

  def computeOldestEntry(filename: String): TaxParcel = { //18
    val oldest = new TaxParcel()
    val inputFile = scala.io.Source.fromFile(filename)
    val lines = inputFile.getLines()
    val keys = lines.next().split(',')
    var min = 3000
    while(lines.hasNext){
      val tmp = new StringBuilder(lines.next())
      var sw = 0; var meet = 0; var met = 0
      for(i <- 0 until tmp.length()){
        if(tmp(i) == '\"'){
          meet += 1
          if(sw != 0 && meet == met*2){
            sw = 0; meet = 0; met = 0
          }
          else if(sw == 0){
            sw = 1
          }
          else if(met == 0 && sw != 0){
            sw = 0
          }
        }
        else if(tmp(i) == ',' && sw == 0){
          tmp.update(i , '^')
          if(meet != 0){
            meet = 0
          }
        }
        else if(tmp(i) == ',' && sw == 1){
          met = meet
        }
      }
      val values = tmp.toString().split('^')

      if(values.length > 20 && values(18).length == 4) {
        if(values(18).toInt < min){
          min = values(18).toInt
          for(i <- 0 until values.length){
            oldest.parcelInfo += (keys(i) -> values(i))
          }
        }
      }
    }
    inputFile.close()
    oldest
  }
  def countPriceRange(filename: String, lower: Int, upper: Int): Int = {
    val inputFile = scala.io.Source.fromFile((filename))
    val lines = inputFile.getLines()
    val headers = lines.next().split(",")
    var cnt = 0
    while(lines.hasNext){
      val tmp = new StringBuilder(lines.next())
      var sw = 0; var meet = 0; var met = 0
      for(i <- 0 until tmp.length()){
        if(tmp(i) == '\"'){
          meet += 1
          if(sw != 0 && meet == met*2){
            sw = 0; meet = 0; met = 0
          }
          else if(sw == 0){
            sw = 1
          }
          else if(met == 0 && sw != 0){
            sw = 0
          }
        }
        else if(tmp(i) == ',' && sw == 0){
          tmp.update(i , '^')
          if(meet != 0){
            meet = 0
          }
        }
        else if(tmp(i) == ',' && sw == 1){
          met = meet
        }
      }
      val values = tmp.toString().split('^')
      if (values.length >= 17 && values != "" && values != "NA"){
        if(values(16).toInt >= lower && values(16).toInt < upper){
          cnt += 1
        }
      }
    }
    inputFile.close()
    cnt
  }
}
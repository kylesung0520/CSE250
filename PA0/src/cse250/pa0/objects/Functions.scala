/**
 * cse250.pa0.objects.Functions.scala
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

object Functions {
  def genNum(n: Int): Int = {
    if(n < 0) {
      return -1
    }
    else {
      n
    }
  }

  def genSeq(n: Int): Seq[Int] = {
    var a : Seq[Int] = Seq()
    for(i <- 0 to n){
      if(i != 0) a =  a :+ i
    }
    a
  }

  def funThree(n: Int): Int = {
    if(n == 9) {
      n - 2
    }
    else if(n == 10){
      n - 4
    }
    else{
      n
    }
  }

  def mapSum(n: Int, f: Int => Int): Int = {
    var result = 0
    for (i <- 1 to n) {
      result += f(i)
    }
    result
  }
}

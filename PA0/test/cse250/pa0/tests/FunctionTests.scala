/**
 * cse250.pa0.tests.FunctionTests.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 */
package cse250.pa0.tests

import cse250.pa0.objects.Functions
import org.scalatest.FlatSpec

class FunctionTests extends FlatSpec {
  // Tests for problem 1.
  behavior of "FunctionsTest.genNum"

  it should "return the non-positve numbers in the range 0 to n" in {
    for(i <- 0 to 1000){
      assert(Functions.genNum(i) == i)
    }
  }


  // Tests for problem 2.
  behavior of "FunctionsTest.genSeq"

  it should "return positive item in seq and length of seq equal to n" in {
    for(n <- 0 to 1000){
      Functions.genSeq(n).foreach(i => assert(i > 0))
    }
    for(n <- 0 to 1000){
      assert(Functions.genSeq(n).length == n)
    }
  }

  // Tests for problem 3.
  behavior of "FunctionsTest.funThree"

  it should "not decrease first" in {
    val first = Functions.funThree(1)
    val second = Functions.funThree(2)
    assert(first < second)
  }
  it should "have only one turning point" in{
    var check = false
    var turningP = 0
    for(n <- 1 to 10){
      if(n > Functions.funThree(n) && Functions.funThree(n) < n-1){
        if(check == false){
          turningP += 1
        }
        check = true
      }
      else{
        if(check == true) {
          turningP += 1
        }
        check = false
      }
    }
    assert(turningP == 1)
  }

  // Tests for problem 4.
  behavior of "FunctionsTest.mapSum"

  def if0(x: Int) = 0
  it should "return 0 if n is 0" in {
    for(n <- 0 to 1000){
      assert(Functions.mapSum(n, if0) == 0)
    }
  }

  def if1(x: Int) = 1
  it should "return every number from 0 to 1 if n is 1" in{
    for(n <- 0 to 1000){
      assert(Functions.mapSum(n, if1) == n)
    }
  }

  def greater(x: Int) = x * x
  it should "return sum of every squared number from 0 to n" in {
    for(n <- 0 to 1000){
      val result = (n * (n + 1) * (2 * n + 1))/6
      assert(Functions.mapSum(n, greater) == result)
    }
  }
}

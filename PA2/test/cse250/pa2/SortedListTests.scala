/**
 * cse250.pa2.SortedListTests.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 */
package cse250.pa2

import cse250.adaptors.LectureQueue
import org.scalatest.{BeforeAndAfter, FlatSpec}
import org.scalatest.Assertions._

class SortedListTests extends FlatSpec with BeforeAndAfter {

  behavior of "apply"
  it should "return the elem of list at index" in {
    val myList = new SortedList[Int]
    myList.insert(1)
    val idx = 0
    //assertThrows[IllegalArgumentException](idx >= myList.length)
    assert(myList(idx) == myList.apply(idx))
  }

  behavior of "iterator"
  it should "visit all elems in list only once" in {
    val myList = new SortedList[Int]
    for(i <- 1 to 5){
      myList.insert(i)
    }
    val complist = new SortedList[Int]
    val iter = myList.iterator
    for(i <- 0 until myList.length){
      assert(iter.hasNext)
      val e = iter.next()
      print(e + " ")
      assert(myList.contains(e))
      complist.insert(e)
    }
    assert(!iter.hasNext)
    for(i <- 0 until complist.length){
      if(i != complist.length-1){
        assert(complist(i) <= complist(i+1))
      }
    }
  }

  behavior of "length"
  it should "return the length of list" in {
    val myList = new SortedList[Int]
    var expected = 0
    for(i <- 1 to 5){
      myList.insert(i)
      expected += 1
    }
    println(myList.length + " " + expected)
    assert(myList.length == expected)
  }

  behavior of "insert"
  it should "insert a solo element into list at index 0" in {
    val myList = new SortedList[Int]
    val valToInsert = 5
    myList.insert(valToInsert)
    assert(myList.length == 1)
    assert(myList(0) == valToInsert)
    println(myList.apply(0))
  }
  it should "insert non-decreasing order" in {
    val myList = new SortedList[Int]
    myList.insert(5)
    myList.insert(2)
    myList.insert(2)
    myList.insert(3)
    myList.insert(2)
    myList.insert(1)

    for(i <- 0 until myList._storageList.length){
      println(myList._storageList(i))
    }
    for(i <- 0 until myList.length){
      if(i != myList.length - 1){
        assert(myList(i) <= myList(i+1))
      }
    }
  }

  behavior of "remove"
  it should "remove" in {
    val myList = new SortedList[Int]
    assert(myList.remove(0) == 0)

    myList.insert(1)
    myList.insert(2)
    myList.insert(2)
    myList.insert(3)

    var cnt = 0
    for(i <- 0 until myList.length){
      if(myList(i) == 2){
        cnt += 1
      }
    }
    assert(myList.remove(4) == 0)
    assert(myList.remove(2) == cnt)
    assert(!myList.contains(2))
    assert(myList.remove(2) == 0)
  }

  behavior of "processBatch"
  it should "process two insertions" in {
    val myList = new SortedList[Int]
    val jobQueue = new LectureQueue[(String,Int)]
    jobQueue.enqueue("insert",0)
    jobQueue.enqueue("insert",0)
    myList.processBatch(jobQueue)
    // Should have inserted the values: 0,0.
    assert(myList.length == 2)
    assert(myList(0) == 0)
    assert(myList(1) == 0)
    // Should have removed both copies of 0.
    jobQueue.enqueue("remove", 0)
    myList.processBatch(jobQueue)
    assert(myList.length == 0)
  }

  it should "process two insertions and then undo both" in {
    val myList = new SortedList[Int]
    myList.insert(1)
    myList.insert(2)
    myList.insert(2)
    myList.insert(2)
    myList.insert(3)
    myList.insert(3)
    val jobQueue = new LectureQueue[(String,Int)]
    jobQueue.enqueue("remove",2)
    jobQueue.enqueue("remove",3)
    myList.processBatch(jobQueue)
    for(i <- myList){
      println(i)
    }
    // Should have inserted the values: 0,0.
    //assert(myList.length == 2)
    //assert(myList(0) == 0)
    //assert(myList(1) == 0)
    // Should have removed both copies of 0.
    //myList.undoLastModification()
    //jobQueue.enqueue("remove", 1)
    //myList.processBatch(jobQueue)

    myList.undoLastModification()
    for(i <- myList){
      println(i + " j ")
    }
    assert(myList.length == 6)
  }
}

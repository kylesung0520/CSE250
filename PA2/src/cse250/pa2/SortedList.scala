/**
 * cse250.pa2.SortedList.scala
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

import cse250.list.{ImmutableLinkedList,EmptyList,ListNode}
import cse250.adaptors.{LectureQueue,LectureStack}
import cse250.objects.TaxParcel


class SortedList[A] (implicit _comp: Ordering[A]) extends collection.Seq[A] {
  // Updates the toString to mention our class name instead of Seq.
  override protected[this] def className = "SortedList"

  // Use _storageList to maintain the sorted list.
  var _storageList: cse250.list.ImmutableLinkedList[A] = cse250.list.EmptyList
  // ---------- MAKE CHANGES BELOW ----------
  // You may add member variables as you wish.

  /** Gets element at position idx within the list. */
  override def apply(idx: Int): A = {
    _storageList.apply(idx)
  }

  /** Gets the number of elements stored within the list. */
  override def length: Int = {
    _storageList.length
  }

  /** Returns an Iterator that can be used only once. */
  override def iterator: Iterator[A] = {
    _storageList.iterator
  }

  /**
   * Inserts one copy of elem into the list in non-decreasing order.
   * @param elem element to be inserted.
   */
  def insert(elem: A): Unit = {
    //_storageList = ListNode[A](elem, _storageList)
    var sw = 0
    if(_storageList.isEmpty){
      _storageList = _storageList.inserted(0,elem)
    }
    else {
      for (i <- 0 until _storageList.length) {
        if (sw == 0 && _comp.compare(elem, _storageList(i)) < 0) {
          _storageList = _storageList.inserted(i, elem)
          sw = 1
        }
      }
      if(sw == 0){
        _storageList = _storageList.inserted(_storageList.length, elem)
      }
    }
  }

  /**
   * Removes all copies of elem from the list.
   * @param elem element to be removed.
   * @return the number of copies removed.
   */
  def remove(elem: A): Int = {
    var cnt = 0
    var tmp: cse250.list.ImmutableLinkedList[A] = cse250.list.EmptyList
    if(_storageList.length == 0){
      0
    }
    else{
      for(i <- 0 until _storageList.length){
        if(_comp.compare(elem, _storageList(i)) != 0){
          tmp = tmp.inserted(tmp.length,_storageList(i))
        }
        else{
          cnt += 1
        }
      }
      _storageList = tmp
      cnt
    }
  }

  /** Takes in a queue of valid operations to perform. Each pair has the form:
   *      (OP,elem)
   *  where:
   *      OP will be the string "insert" or "remove"
   *      elem will be a value of type A to use as the argument to OP. */
  var tmpStack : cse250.types.mutable.StackADT[(String, A)] = new LectureStack[(String, A)]
  var cntStack = new LectureStack[Int]
  var sw = 0
  var cnt = 0
  def processBatch(operations: cse250.types.mutable.QueueADT[(String,A)]): Unit = {
      while(!operations.isEmpty){
        val a = operations.dequeue
        tmpStack.push(a._1,a._2)
        if(a._1 == "insert"){
          insert(a._2)
        }
        else if(a._1 == "remove"){
          cnt = remove(a._2)
          if(cnt == 0){
            sw = 1
          }
          else{
            cntStack.push(cnt)
          }
        }
    }
  }

  /** Undo the last modification, if any change has been made.
   *  If no change to undo exists, raise an IllegalArgumentException.
   */
  def undoLastModification(): Unit = {
    if(tmpStack.isEmpty){
      throw new IllegalArgumentException
    }
    else{
      while(!tmpStack.isEmpty){
        val b = tmpStack.pop
        if(b._1 == "insert"){
          remove(b._2)
        }
        else if(b._1 == "remove"){
          if(sw == 0){
            for(i <- 0 until cntStack.pop){
              insert(b._2)
            }
          }
          else{
            sw == 0
          }
        }
      }
    }
  }
}

object NewIntOrdering extends Ordering[Int] {
  def compare(x: Int, y: Int): Int = {
    if(x % 2 == 0 && y % 2 == 1){
      -1
    }
    else if (x % 2 == 0 && y % 2 == 0) {
      if (x > y) {
        -1
      }
      else if (x < y) {
        1
      }
      else {
        0
      }
    }
    else if (x % 2 == 1 && y % 2 == 1) {
      if (x > y) {
        -1
      }
      else if (x < y) {
        1
      }
      else {
        0
      }
    }
    else {
      1
    }
  }
}

object TaxParcelStreetGroupingOrdering extends Ordering[TaxParcel] {
  def compare(x: TaxParcel, y: TaxParcel): Int = {
    val a = x.parcelInfo("STREET")
    val b = y.parcelInfo("STREET")

    if(a == b){
      0
    }
    else if(a < b){
      -1
    }
    else{
      1
    }
  }
}
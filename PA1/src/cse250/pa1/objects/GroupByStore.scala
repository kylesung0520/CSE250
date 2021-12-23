/**
 * cse250.pa1.GroupByStore.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 */
package cse250.pa1.objects

import cse250.objects.{DNode, TaxParcel}

import scala.collection.mutable.ArrayBuffer

class GroupByStore extends Seq[TaxParcel] {
  // Member/instance variables are defined public for ease of access for testing.
  var _groupings: ArrayBuffer[DNode[TaxParcel]] = new ArrayBuffer[DNode[TaxParcel]]
  var _groupingAttribute: String = "STREET"
  var _numStored = 0

  def apply(i: Int): TaxParcel = {
    val iter = this.iterator
    for (_ <- 0 until i) iter.next()
    iter.next()
  }

  /** Inserts element to head of corresponding grouping list. */
  def insert(taxParcel: TaxParcel): Unit = {
   val node = new DNode[TaxParcel](taxParcel, null, null)
    if(_numStored == 0){
      _groupings += node
      _numStored += 1
    }
    else{
      var sw = 0
      for(i <- 0 until _groupings.length) {
        if (_groupings(i)._value.parcelInfo(_groupingAttribute) == node._value.parcelInfo(_groupingAttribute)) {
          val tmp = _groupings(i)
          _groupings(i) = node
          node._next = tmp
          tmp._prev = node
          _numStored += 1
          sw = 1
        }
      }
      if(sw == 0){
        _groupings += node
        _numStored += 1
        for(j <- 0 until _groupings.length-1){
          if(_groupings(j)._value.parcelInfo(_groupingAttribute) > _groupings(_groupings.length-1)._value.parcelInfo(_groupingAttribute)){
            val tmp = _groupings(j)
            _groupings(j) = _groupings(_groupings.length-1)
            _groupings(_groupings.length-1) = tmp
          }
        }
      }
    }
  }

  /** Regroup . */
  def regroup(attribute: String): Unit = {
    if( _groupingAttribute != attribute) {
      val tmp: ArrayBuffer[DNode[TaxParcel]] = new ArrayBuffer[DNode[TaxParcel]]
      val a = _groupings.iterator
      while (a.hasNext) {
        tmp += a.next()
      }
      _groupingAttribute = attribute
      _groupings.clear()
      _numStored = 0
      for (i <- 0 until tmp.length) {
        var b = tmp(i)
        insert(b._value)
        while (b._next != null) {
          b = b._next
          insert(b._value)
        }
      }
    }
  }

  /** Returns an Iterator to all entries that can be used only once. */
  def iterator: Iterator[TaxParcel] = new Iterator[TaxParcel] {
    var idx = 0
    var curr : DNode[TaxParcel] = null
    if(_numStored > 0){
      curr = _groupings(idx)
    }

    override def hasNext: Boolean = {
        if(idx < _groupings.length-1 && curr == null){
          idx += 1
          curr = _groupings(idx)
        }
        curr != null
    }

    override def next(): TaxParcel = {
      val retVal = curr._value
        curr = curr._next
      retVal
      }
  }

  /** Returns an Iterator to only the entries with matching values on the grouping attribute that can be used only once. */
  def iterator(value: String): Iterator[TaxParcel] = new Iterator[TaxParcel] {
    var curr : DNode[TaxParcel] = null
    var sw = 0
    if(_numStored != 0){
      for(i <- 0 until _groupings.length){
        if(_groupings(i)._value.parcelInfo(_groupingAttribute) == value){
          curr = _groupings(i)
          sw = 1
        }
      }
    }
    override def hasNext: Boolean = {
        if(sw == 0){
          false
        }
        else{
          curr != null
        }
    }
    override def next(): TaxParcel = {
      val retVal = curr._value
      curr = curr._next
      retVal
    }
  }

  def length: Int = _numStored

  override def toString: String = this.iterator.mkString("GroupByStore(", "\n", ")")
}

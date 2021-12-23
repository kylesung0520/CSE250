/**
 * cse250.pa4.HashTableMap.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 */
package cse250.pa4

import cse250.examples.types.mutable.Map
import scala.collection.mutable.ListBuffer
import scala.util.hashing.Hashing

class HashTableMap[K, V](val alphaMax: Double = 0.6)(implicit hash: Hashing[K]) extends Map[K, V] {
  var _n = 0
  var _N = 10
  var _alpha: Double = 0.0
  var _bucketArray = Array.fill[ListBuffer[(K, V)]](_N)(ListBuffer[(K, V)]())

  def rehash(newSize: Int): Unit = {
    if (newSize > _N) {
      val oldBucketArray = _bucketArray
      _n = 0
      _N = newSize
      _alpha = 0.0
      _bucketArray = Array.fill(_N)(ListBuffer[(K, V)]())
      for (bucket <- oldBucketArray; elem <- bucket) this.addOne(elem)
    }
  }

  override def get(key: K): Option[V] = {
    val lookupIndex = hash.hash(key) % _N
    _bucketArray(lookupIndex).find(elem => elem._1 == key) match {
      case Some(elem) => Some(elem._2)
      case None       => None
    }
  }

  override def addOne(elem: (K, V)): Unit = {
    val index = hash.hash(elem._1) % _N
    if(_bucketArray(index).isEmpty){
      if(_alpha == alphaMax){
        rehash(_N*2)
        _n += 1
      }
      _bucketArray(index).prepend(elem)
      _n += 1
      _alpha = _n.toDouble/_N
    }
    else{
      if(_bucketArray(index).head._1 == elem._1){
        _bucketArray(index).update(0, elem)
      }
    }
  }

  override def removeOne(key: K): Boolean = {
    val index = hash.hash(key) % _N
    if(_bucketArray(index).isEmpty){
      false
    }
    else{
      _bucketArray(index).remove(0)
      _n -= 1
      _alpha = _n.toDouble / _N
      true
    }
  }

  override def iterator: Iterator[(K, V)] = new Iterator[(K,V)]{
    var currpos = 0
     override def hasNext: Boolean = {
       if(currpos < _N-1 && _bucketArray(currpos+1).nonEmpty){
         true
       }
       else{
         false
       }
    }
    override def next(): (K,V) ={
      var retVal = _bucketArray(currpos)
        if(retVal.isEmpty){
          currpos += 1
          retVal = _bucketArray(currpos)
        }
        else{
          currpos += 1
          if(_bucketArray(currpos).isEmpty){
            while(_bucketArray(currpos).isEmpty && currpos < _N){
              currpos += 1
            }
          }
          retVal = _bucketArray(currpos)
        }
      retVal.head
    }
  }
}

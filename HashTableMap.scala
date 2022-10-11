/**
 * cse250.pa5.HashTableMap.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT: arflierl
 * Person#: 31245340
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:bmb4
 */
package cse250.pa4

import cse250.examples.types.mutable.Map
import scala.collection.mutable.ListBuffer
import scala.util.hashing.Hashing
//reference: https://alvinalexander.com/scala/how-to-create-mutable-list-in-scala-listbuffer-cookbook/
class HashTableMap[K, V](_alphaMax: Double = 0.6)(implicit hash: Hashing[K]) extends Map[K, V] {
  var _n = 0
  var _N = 10
  var _alpha: Double = 0.0
  var _bucketArray = Array.fill[ListBuffer[(K, V)]](_N)(ListBuffer[(K, V)]())

  def rehash(newSize: Int): Unit = {
    println("rehashing")
    if (newSize > _N) {
      val oldBucketArray = _bucketArray
      _n = 0
      _N = newSize
      _alpha = 0.0
      _bucketArray = Array.fill(_N)(ListBuffer[(K, V)]())
      for (bucket <- oldBucketArray; elem <- bucket) addOne(elem)
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
    var insertIndex = hash.hash(elem._1) % _N
    var bucketIndex = 0
    for (i <- _bucketArray(insertIndex)) {
      if (i._1 == elem._1) {
        _bucketArray(insertIndex)(bucketIndex) = elem
        return  //no need to increase _n for this case
      }
      bucketIndex += 1
    }
    _n += 1 //now I am certain the we are inserting something new, so increment n and check _alpha
    _alpha = _n.toDouble/_N
    if (_alpha > _alphaMax) { //if rehashing is necessary
      rehash(_N + _N) //creates a new _N value -> update insertIndex
      _n += 1 //because it was reset in rehash
      _alpha = _n.toDouble/_N
      insertIndex = hash.hash(elem._1) % _N
      _bucketArray(insertIndex).prepend(elem)
    }
    else {
      // _n has already been incremented
      // _N has not changed because rehashed wasn't called
      // insert index only depends on elem and _N, which have not changed since initial assignment
      _bucketArray(insertIndex).prepend(elem)
      // nothing left to do here
    }
  }

  override def removeOne(key: K): Boolean = {  //overall method design from lecture slides
    val removeIndex = hash.hash(key) % _N
    for (i <- _bucketArray(removeIndex)) {
      if (i._1 == key) {
        _bucketArray(removeIndex) -= i
        _n -= 1
        _alpha = (_n.toDouble/_N)
        return true
      }
    }
    false
  }

  override def iterator: Iterator[(K, V)] = new Iterator[(K, V)] {  //overall structure from lecture code examples
    var curArrayIdx = 0
    var curBucketIdx = 0
    while (_n > 0 && curArrayIdx < _N && _bucketArray(curArrayIdx) == ListBuffer[(K, V)]()) curArrayIdx += 1
    override def hasNext: Boolean = {
      if (_n == 0) return false
      if (curArrayIdx >= _N) return false
      if (_bucketArray(curArrayIdx).length > curBucketIdx)  true
      else {
        curBucketIdx = 0
        var found = false
        while (curArrayIdx < _N  && !found) {
          curArrayIdx += 1
          if (curArrayIdx == _N) return false
          if (_bucketArray(curArrayIdx) != ListBuffer[(K, V)]()) {
            found = true
          }
        }
        if (curArrayIdx >= _N) false
        else true
      }
    }

    override def next: (K, V) = {
      val retval = _bucketArray(curArrayIdx)(curBucketIdx)
      curBucketIdx += 1
      retval
    }
  }
}
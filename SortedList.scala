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
 * Submission author
 * UBIT: arflierl
 * Person#: 31245340
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT: bmb4@buffalo.edu
 */
package cse250.pa2

import cse250.list.{EmptyList, ListNode}
import cse250.types.mutable.QueueADT

class SortedList[A] (implicit _comp: Ordering[A]) extends collection.Seq[A] {
  // Updates the toString to mention our class name instead of Seq.
  override protected[this] def className = "SortedList"

  // Use _storageList to maintain the sorted list.
  var _storageList: cse250.list.ImmutableLinkedList[A] = cse250.list.EmptyList
  // ---------- MAKE CHANGES BELOW ----------
  var changes = new cse250.adaptors.LectureStack[cse250.list.ImmutableLinkedList[A]]
  var queue = false
  /** Gets element at position i within the list. */
  override def apply(i: Int): A = {
    if (_storageList.isEmpty || i > (_storageList.length - 1)) {
      throw new IllegalArgumentException("index out of bounds")
    } else _storageList.apply(i)
  }

  /** Gets the number of elements stored within the list. */
  override def length: Int = _storageList.length

  /** Returns an Iterator that can be used only once. */
  override def iterator: Iterator[A] = _storageList.iterator

  /**
   * Inserts one copy of elem into the list in non-decreasing order.
   * @param elem element to be inserted.
   */
  def insert(elem: A): Unit = {
    if (_storageList.isEmpty) { //emptyList
      if (!queue) changes.push(_storageList)
      _storageList = _storageList.inserted(0, elem)
      return
    }
    var check = _storageList
    if (check.tail.isEmpty) { //one entry in the list
      if (_comp.gt(elem, _storageList.head)) {
        if (!queue) changes.push(_storageList)
        _storageList = _storageList.inserted(1, elem)
      }
      else {
        if (!queue) changes.push(_storageList)
        _storageList = _storageList.inserted(0, elem)
      }
    }
    else { //more than one entry in the list
      var insertTo = 0
      while (!check.tail.isEmpty) { //while there is more than one element
        if (_comp.gt(elem, check.head)) insertTo += 1
        check = check.tail
      }
      if (_comp.gt(elem, check.head)) insertTo += 1  //check with the last element
      if (!queue) changes.push(_storageList)
      _storageList = _storageList.inserted(insertTo, elem)
    }
  }


  /**
   * Removes all copies of elem from the list.
   * @param elem element to be removed.
   * @return true if any change has been made, and false otherwise.
   */
  def remove(elem: A): Boolean = {
    if (_storageList.isEmpty) return false
    if (_storageList.tail.isEmpty) { //if there is only one element in the list
      if (_storageList.head == elem) {
        if (!queue) changes.push(_storageList)
        _storageList = cse250.list.EmptyList
        return true
      }
    }
    var retval = false
    var rangeStart = 0
    var rangeEnd = 0
    var first = false
    var count = 0
    var check = _storageList
    var done = false
    var removed = 0
    while (!done) {
      count += 1
      if (_comp.equiv(check.head, elem)) {  //check if the head of check is equivalent to the function argument
        if (!first) {  //if it is, and we haven't seen it before, assign the start of the range of indices
          rangeStart = count - 1
          first = true
        }
      }
      if (first && _comp.gt(check.head, elem)) {  //if we have seen it before and the next list element is gt
        retval = true
        rangeEnd = count - 1
        if (!queue) changes.push(_storageList)
        for (i<- rangeStart until rangeEnd) {
          _storageList = _storageList.removed(i - removed)
          removed += 1
        }
        return retval
      }
      if (!check.tail.isEmpty) {
        check = check.tail
      }else { //if the tail of the check is empty
        if (!check.isEmpty) {
          if (first && check.head == elem) { //if we have seen this number once and the current head is equal
            if (!queue) changes.push(_storageList)
            retval = true
            var gone = 0
            for (i <- rangeStart until count) {
              _storageList = _storageList.removed(i - gone)
              gone += 1
            }
            done = true
          } else { //we have gone through the entire container and no elements are equiv to function argument
            done = true
          }
        }
      }
    }
    retval
  }

  /** Takes in a queue of valid operations to perform. Each pair has the form:
   *      (OP,elem)
   *  where:
   *      OP will be the string "insert" or "remove"
   *      elem will be a value of type A to use as the argument to OP. */
  def processBatch(operations: cse250.types.mutable.QueueADT[(String,A)]): Unit = {
    queue = true
    changes.push(_storageList)
    while (!operations.isEmpty) {
      val job = operations.front._1
      if (job != "insert" && job != "remove") throw new IllegalArgumentException("invalid operation, 'insert' or 'remove' only!")
      else if (job == "insert") this.insert(operations.front._2)
      else this.remove(operations.front._2)
      operations.dequeue
    }
    queue = false
  }


  /** Undo the last modification, if any change has been made.
   * If no change to undo exists, throw an IllegalArgumentException.
   */
  def undoLastModification(): Unit = {
    if (changes.isEmpty) {
      throw new IllegalArgumentException("nothing to be undone")
    }else _storageList = changes.pop
  }
}

/**
 * cse250.pa1.DataEntryStore.scala
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
 * UBIT:
 */
package cse250.pa1

import cse250.objects.{EmbeddedEmpty, EmbeddedListNode, EmbeddedNode}

class DataEntryStore[A](private val _capacity: Int = 100)
  extends collection.mutable.Seq[A] {
  // These private members should not be modified.
  private val _emptyNode = new EmbeddedEmpty[A]
  private val _dataArray = Array.fill[EmbeddedListNode[A]](_capacity)(_emptyNode)
  private var _headIndex = -1
  private var _tailIndex = -1
  private var _numStored = 0

  // Public getters for private members.
  def dataArray: Array[EmbeddedListNode[A]] = _dataArray
  def headIndex: Int = _headIndex
  def tailIndex: Int = _tailIndex
  def emptyNode: EmbeddedEmpty[A] = _emptyNode

  /** Inserts element to tail of list. */
  def insert(elem: A): Unit = {

    val entry = new EmbeddedNode[A](elem, -1, -1)

    if (_numStored == 0) {
      _headIndex = 0
      _tailIndex = 0
      _numStored += 1
      _dataArray(0) = entry
    } else if (_numStored < _capacity) {
      entry.prev = _tailIndex //new element points back to the old tail
      for (i <- 0 until _capacity) {
        if (_dataArray(i) == _emptyNode) { //if an empty node exists
          _dataArray(i) = entry //place entry into first occurrence of an emptyNode
          _dataArray(_tailIndex).next = i //the old tail now points to the new tail
          _tailIndex = i
          _numStored += 1
          return
        }
      }
    } else { //this is the case when the capacity has been reached and we must modify the head
      entry.prev = _tailIndex //points back to the old tail
      val newHeadIndex:Int = _dataArray(_headIndex).next //this index will become the new head
      _dataArray(_headIndex) = entry //replaced the slot for the OLD head
      _dataArray(_tailIndex).next = _headIndex //entry which was the tail now has next pointing to this new tail
      _tailIndex = _headIndex //this new entry is now the tail
      _headIndex = newHeadIndex //the headIndex is now updated
      _dataArray(_headIndex).prev = -1
    }
  }

  /** Removes all copies of the given element. */
  def remove(elem: A): Boolean = {
    if (_numStored == 0) return false //if there is nothing stored, return false
    var elementPresent: Boolean = false  //this indicates if we have found the requested element
    var cur = _dataArray(_headIndex)  //this is the placeholder to keep track of where we currently are in linked list
    for (i <- iterator) {
      if (i != elem) {
        if (cur.next != -1) {
          cur = _dataArray(cur.next)
        }else return elementPresent
      } else {
        elementPresent = true
        if (cur.prev == -1 && cur.next == -1) {  //this is the case when removing head and tail (only element)
          _dataArray(_headIndex) = emptyNode
          _numStored -= 1
          _headIndex = -1
          _tailIndex = -1
        }else if (cur.next != -1 && cur.prev == -1) {  //this is the case when removing head, not tail
          _dataArray(cur.next).prev = cur.prev //assign the prev of node being removed to the next node
          val tmp = cur.next  //temporary placeholder
          _dataArray(_headIndex) = emptyNode //replace the head node with an empty node
          _headIndex = tmp //if this was the head, update the headIndex
          cur = _dataArray(_headIndex)  //reassign cur to the new head
          _numStored -= 1
        }else if (cur.prev != -1 && cur.next == -1) {  //this is the case removing the tail, not the head
          _dataArray(cur.prev).next = cur.next  //assigning the previous node next value to cur.next (-1 here)
          val tmp = cur.prev  //temporary placeholder
          _dataArray(_tailIndex) = emptyNode  //the old tail is replaced with an empty node in the _dataArray
          _tailIndex = tmp  //the new _tailIndex is assigned the value saved in temporary placeholder
          cur = _dataArray(_tailIndex)  //we move the general placeholder to the new tail
          _numStored -= 1
        }else {  //this is the case of removing a middle element
          val tmp = _dataArray(cur.prev).next //this is the _dataArray index of the element to remove
          _dataArray(cur.prev).next = cur.next  //link previous element to next element
          _dataArray(cur.next).prev = cur.prev  //link next element to previous element
          //no need to change the head OR the tail
          cur = _dataArray(cur.next)  //move general placeholder to the next node
          _dataArray(tmp) = _emptyNode  //replace element in _dataArray with an empty node
          _numStored -= 1
        }
      }
    }
    elementPresent
  }
  /** Returns the count of nodes containing given entry. */
  def countEntry(entry: A): Int = {
    var count = 0
    for (i <- iterator) {
      if (i == entry) {
        count += 1
      }
    }
    count
  }

  /** Gets the element at the specified index. */
  override def apply(idx: Int): A = {
    require(idx < _capacity)
    require(idx > -1)
    var currentIndex = 0
    for (i <- iterator) {
      if (currentIndex != idx) {
        currentIndex += 1
      }else {
        println("index in apply: ", currentIndex)
        return i
      }
    }
    _emptyNode.value.get
  }

  /** Replaces element at given index with a new value. */
  override def update(idx: Int, elem: A): Unit = {
    require(idx < _capacity)
    require(idx > -1)
    var currentIndex = 0
    var currentElement = _dataArray(_headIndex)
    for (i <- iterator) {
      if (currentIndex != idx) {
        currentIndex += 1
        currentElement = _dataArray(currentElement.next)
      }else {
        currentElement.value = elem
      }
    }
  }

  /** Returns an Iterator that can be used only once. */
  def iterator: Iterator[A] = new Iterator[A] {
    private var currentIndex = _headIndex

    override def hasNext: Boolean = currentIndex != -1

    override def next(): A = {
      val previousIndex = currentIndex
      currentIndex = _dataArray(currentIndex).next
      _dataArray(previousIndex).value.get
    }
  }

  /** Returns the length of the stored list. */
  override def length: Int = _numStored

  override def toString: String = if (_numStored == 0) "" else this.iterator.addString(new StringBuilder, "DataEntryStore: (", ",", ")\n").result()
}

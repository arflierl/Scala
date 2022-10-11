/**
 * cse250.pa3.Trie.scala
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
package cse250.pa3

import cse250.examples.graph.AdjacencyListGraph
import scala.math.Ordering

class Trie {
  /** The graph to store the trie. */
    /**REFERENCES
     * https://piazza.com/class/k5muciwojsl7d1?cid=771
     * https://piazza.com/class/k5muciwojsl7d1?cid=527
     * week 12 recitation slides
     * graph & PQ ADT lecture slides
     * https://www.scala-lang.org/api/current/scala/collection/mutable/PriorityQueue.html
     * */
  val _storageGraph = new AdjacencyListGraph[Int,Char]
  /** The root of the trie. */
  val _trieRoot = _storageGraph.insertVertex(0)
  var _length = 0
  case class wordInfo(word:String, length:Int, count:Int) //from recitation slides
  var wordList = new cse250.examples.list.LectureSinglyLinkedList[wordInfo]
  val lengthOrdering = (getLength:wordInfo) => getLength.length
  val countOrdering = (getCount:wordInfo) => getCount.count
  val pqCount = scala.collection.mutable.PriorityQueue.empty[wordInfo](Ordering.by(countOrdering))
  val pqLength = scala.collection.mutable.PriorityQueue.empty[wordInfo](Ordering.by(lengthOrdering))


  /** Inserts the given word into the trie graph. */
  def insert(word: String): Unit = {
    var curVertex = _trieRoot
    _length += 1
    var newWord = true
    var curIdx = 0
    var wordCount = 0
    for (i <- wordList) {
      if (i.word == word) {
        newWord = false
        wordCount = i.count + 1
        val updateInfo: wordInfo = wordInfo(word, word.length, wordCount)
        wordList.update(curIdx, updateInfo)
      }
      curIdx += 1
    }

    if (newWord) {
      wordCount = 1
      val updateInfo:wordInfo = wordInfo(word, word.length, wordCount)
      wordList.insert(curIdx, updateInfo)
    }

    var stringIndex = 0
    for (char <- word) {
      var found = false
      for (edge <- _storageGraph.incidentEdges(curVertex)) {
        if (edge._elem == char && edge._v != curVertex) {
          curVertex = edge._v
          found = true
        }
      }
      if (!found) {
        val newVertex = _storageGraph.insertVertex(0)
        _storageGraph.insertEdge(curVertex, newVertex, char)
        curVertex = newVertex
      }
      stringIndex += 1
    }
    curVertex._elem += 1
  }

  /** Returns the number of times the given word was inserted. */
  def count(word: String): Int = {
    for (i <- wordList) {
      if (i.word == word) {
        return i.count
      }
    }
    0
  }

  /** Returns the number of words stored within. */
  def length: Int = _length

  /** Returns an Iterator that can be used only once. */
  def iterator: Iterator[String] = new Iterator[String] {
    for (i <- wordList) {
      pqLength.enqueue(i)
    }

    var _currentNode = pqLength.head
    override def hasNext: Boolean = pqLength.nonEmpty

    // Valid as long as hasNext is true.
    override def next: String = {
      val retvalSource = pqLength.dequeue()
      if (pqLength.nonEmpty) _currentNode = pqLength.head
      retvalSource.word
    }
  }

  /** Returns a sequence of all words of a given length ordered alphabetically. */
  def allWordsOfLength(length: Int): Seq[String] = {
    val retval = new cse250.examples.list.LectureSinglyLinkedList[String]
    for (i <- wordList) {
      pqLength.enqueue(i)
    }
    val flipped = pqLength.clone().reverse
    while (flipped.nonEmpty) {
      val check = flipped.dequeue().word
      if (check.length == length) retval.insert(0, check)
    }
    pqLength.dequeueAll
    retval.reverse.toSeq
  }

  /** Returns a sequence containing the k most inserted words.*/
  def mostCommon(k: Int): Seq[String] = {
    for (i <- wordList) {
      pqCount.enqueue(i)
    }
    val retval = new cse250.examples.list.LectureArrayList[String](k)
    var count = k
    while (count > 0) {
      retval.insert(0, pqCount.dequeue.word)
      count -= 1
    }
    if (pqCount.nonEmpty) pqCount.dequeueAll
    retval.toSeq
  }

  /** Returns a sequence containing the k most inserted words that start with the given prefix. */
  def mostCommonWithPrefix(prefix: String, k: Int): Seq[String] = {
    val retval = new cse250.examples.list.LectureArrayList[String](k)
    val pqTie = scala.collection.mutable.PriorityQueue.empty[String]
    var matches = 0
    for (i <- wordList) {
      if (i.length >= prefix.length && i.word != "") {
        var _match = true
        matches += 1
        for (j <- 0 until prefix.length) {
          if (i.word(j) != prefix(j)) _match = false
        }
        if (_match) pqCount.enqueue(i)
      } else {
        if (prefix == "") pqCount.enqueue(i)
      }
    }
    var count = k
    if (count >= matches) {
      while (count > 0) {
        if (pqCount.nonEmpty) retval.insert(0, pqCount.dequeue.word)
        count -= 1
      }
    }else {
      for (i <- 0 until matches) {
        if (pqCount.nonEmpty) pqTie.enqueue(pqCount.dequeue().word)
      }
      while (count > 0 ) {
        if (pqTie.nonEmpty) retval.insert(0, pqTie.dequeue())
        count -= 1
      }
    }
    if (pqCount.nonEmpty) pqCount.dequeueAll
    if (pqTie.nonEmpty) pqTie.dequeueAll
    if (pqLength.nonEmpty) pqLength.dequeueAll
    retval.reverse.toSeq
    }
}

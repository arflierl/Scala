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
 * Submission author
 * UBIT: arflierl
 * Person#: 31245340
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa2

import cse250.adaptors.LectureQueue
import org.scalatest.{BeforeAndAfter, FlatSpec}
import org.scalatest.Assertions._

class SortedListTests extends FlatSpec with BeforeAndAfter {

  behavior of "afternoon destroyer"
  it should "stop worrying me" in {
    val myList = new SortedList[Int]
    for (i <- 0 until 6) myList.insert(i)
    myList.insert(0)
    myList.insert(0)
    myList.insert(0)
    myList.insert(0)
    myList.insert(0)
    myList.insert(0)
    myList.insert(0)
    myList.insert(0)
    myList.insert(0)
    myList.insert(6)
    myList.insert(6)
    myList.insert(6)
    myList.insert(6)
    myList.insert(6)
    myList.insert(6)
    myList.insert(6)
    myList.insert(6)
    myList.insert(6)
    myList.insert(6)
    myList.insert(6)
    myList.insert(-1)
    myList.insert(-1)
    myList.insert(-1)
    myList.insert(-1)
    myList.insert(-1)
    myList.insert(-1)
    myList.insert(-1)
    myList.insert(-1)
    myList.insert(-1)
    myList.insert(-1)
    myList.insert(-1)
    myList.insert(-1)
    myList.insert(5)
    myList.insert(5)
    myList.insert(5)
    myList.insert(5)
    myList.insert(5)
    myList.insert(5)
    myList.insert(5)
    myList.insert(5)
    myList.insert(5)
    myList.insert(5)
    myList.insert(5)
    myList.insert(4)
    myList.insert(4)
    myList.insert(4)
    myList.insert(4)
    myList.insert(4)
    myList.insert(4)
    myList.insert(4)
    myList.insert(4)
    myList.insert(4)
    myList.insert(4)
    myList.insert(4)
    myList.insert(4)
    println(myList.length)
    println(myList.toString())
    println("")
    println("")
    println("removing 6")
    myList.remove(6)
    println(myList.toString())
    println("")
    assert(myList.length == 50)
    println("removing -1")
    println("")
    myList.remove(-1)
    println(myList.toString())
    println("")
    assert(myList.length == 38)
    assert(myList.head == 0)
    println("removing 4")
    println("")
    myList.remove(4)
    println(myList.toString())
    assert(myList.length == 25)
    println("removing 0")
    println("")
    myList.remove(0)
    println(myList.toString())
    println("")
    assert(myList.head == 1)
    assert(myList.length == 15)
    println("removing 5")
    println("")
    myList.remove(5)
    println(myList.toString())
    println("")
    assert(myList.length == 3)
  }

  behavior of "why man why!"
  it should "hopefully work" in {
    val myList = new SortedList[Int]
    val jobQueue = new LectureQueue[(String,Int)]
    for (i <- 0 until 200) {
      jobQueue.enqueue("insert", 11)
      jobQueue.enqueue("insert", 10)
      jobQueue.enqueue("insert", 0)
      jobQueue.enqueue("insert", 20)
      jobQueue.enqueue("insert", -200)
      jobQueue.enqueue("insert", -7)
      jobQueue.enqueue("insert", 99)
      jobQueue.enqueue("insert", 15)
    }
    myList.processBatch(jobQueue)
    println(myList.toString())
    assert(myList.length == 1600)
    assert(myList.head == -200)
    println("")
    jobQueue.enqueue("remove", 99)
    println("removing 99 using a batch call")
    myList.processBatch(jobQueue)
    println(myList.toString())
    assert(myList.length == 1400)
    assert(myList.head == -200)
    println("")
    jobQueue.enqueue("remove", -200)
    println("removing 99 using a batch call")
    myList.processBatch(jobQueue)
    println(myList.toString())
    assert(myList.length == 1200)
    assert(myList.head == -7)
    println("")
    myList.undoLastModification()
    println(myList.toString())
    assert(myList.length == 1400)
    assert(myList.head == -200)
    println("")
    myList.undoLastModification()
    println(myList.toString())
    assert(myList.length == 1600)
    assert(myList.head == -200)
    println("")
    myList.undoLastModification()
    println(myList.toString())
    assert(myList.isEmpty)
    println("")
    for (i <- 0 until 200) {
      jobQueue.enqueue("insert", 11)
      jobQueue.enqueue("insert", 10)
      jobQueue.enqueue("insert", 0)
      jobQueue.enqueue("insert", 20)
      jobQueue.enqueue("insert", -200)
      jobQueue.enqueue("insert", -7)
      jobQueue.enqueue("insert", 99)
      jobQueue.enqueue("insert", 15)
    }
    jobQueue.enqueue("remove", 11)
    jobQueue.enqueue("remove", 10)
    jobQueue.enqueue("remove", 0)
    jobQueue.enqueue("remove", 20)
    jobQueue.enqueue("remove", -200)
    jobQueue.enqueue("remove", -7)
    jobQueue.enqueue("remove", 99)
    jobQueue.enqueue("insert", 15)
    myList.processBatch(jobQueue)
    println(myList.toString())
    assert(myList.length == 201)
    assert(myList.head == 15)
    println("")
    myList.remove(15)
    assert(myList.isEmpty)
    myList.undoLastModification()
    println(myList.toString())
    assert(myList.length == 201)
    assert(myList.head == 15)
    println("")
    myList.undoLastModification()
    println(myList.toString())
    assert(myList.isEmpty)
  }

  behavior of "trouble maker"
  it should "stop failing" in {
    val myList = new SortedList[Int]
    myList.insert(1)
    myList.insert(2)
    for (i <- 0 until 5) myList.insert(3)
    println("list after inserting 5 3's should be (1,2,3,3,3,3,3)")
    println("")
    println(myList.toString())
    println("")
    println("inserting 4")
    myList.insert(4)
    println("")
    assert(myList.length == 8)
    println("list after inserting 4 should be (1,2,3,3,3,3,3,4)")
    println("")
    println(myList.toString())
    println("")
    myList.undoLastModification()
    println("list after undoMod should be (1,2,3,3,3,3,3)")
    println("")
    println(myList.toString())
    println("")
    println("inserting 4")
    myList.insert(4)
    println("")
    assert(!myList.remove(789))
    myList.remove(879)
    assert(myList.length == 8)
    println("list after inserting 4 should be (1,2,3,3,3,3,3,4)")
    println("")
    println(myList.toString())
    println("")
    println("removing 3")
    println("")
    myList.remove(3)
    println("")
    println("list after removing 3 should be (1,2,4)")
    println(myList.toString())
    println("")
    assert(myList.length == 3)
    println("list after undo modification should be (1,2,3,3,3,3,3,4)")
    println("")
    myList.undoLastModification()
    println("")
    println(myList.toString())
    println("")
    assert(myList.length == 8)
    println("list after undo modification should be (1,2,3,3,3,3,3)")
    println("")
    myList.undoLastModification()
    println("")
    println(myList.toString())
    println("")
    println("list after undo modification should be (1,2,3,3,3,3)")
    println("")
    myList.undoLastModification()
    println("")
    println(myList.toString())
    println("")
    println("list after undo modification should be (1,2,3,3,3)")
    println("")
    myList.undoLastModification()
    println("")
    println(myList.toString())
    println("")
    println("list after undo modification should be (1,2,3,3)")
    println("")
    myList.undoLastModification()
    println("")
    println(myList.toString())
    println("")
    println("list after undo modification should be (1,2,3)")
    println("")
    myList.undoLastModification()
    println("")
    println(myList.toString())
    println("")
    println("list after undo modification should be (1,2)")
    println("")
    myList.undoLastModification()
    println("")
    println(myList.toString())
    println("")
    println("list after undo modification should be (1)")
    println("")
    myList.undoLastModification()
    println("")
    println(myList.toString())
    println("")
    println("list after undo modification should be ()")
    println("")
    myList.undoLastModification()
    println("")
    println(myList.toString())
    println("")
  }

  behavior of "easy undo"
  it should "handle this" in {
    val myList = new SortedList[Int]
    myList.insert(0)
    myList.insert(1)
    myList.insert(2)
    myList.undoLastModification()
    println("list should be (0,1) after first undo")
    println("")
    assert(myList.length == 2)
    assert(myList.head == 0)
    println(myList.toString())
    println("")
    myList.undoLastModification()
    println("list should be (0) after second undo")
    println("")
    assert(myList.length == 1)
    assert(myList.head == 0)
    println(myList.toString())
    println("")
    myList.undoLastModification()
    println("list should be empty after third undo")
    println("")
    assert(myList.isEmpty)
    println(myList.toString())
    println("")

  }

  behavior of "batch undo"
  it should "do what it does" in {
    val myList = new SortedList[Int]
    val jobQueue = new LectureQueue[(String,Int)]
    jobQueue.enqueue("insert", 0)
    jobQueue.enqueue("insert", 1)
    jobQueue.enqueue("insert", 2)
    myList.processBatch(jobQueue)
    assert(myList.length == 3)
    assert(myList.head == 0)
    println("list should be (0,1,2) after calling processBatch")
    println("")
    println(myList.toString())
    println("")
    myList.undoLastModification()
    println("list should be empty after calling undoLastModification")
    println("")
    println(myList.toString())
    println("")
    assert(myList.isEmpty)

  }

  behavior of "easy test suite"
  it should "do what I say" in {
    val myList = new SortedList[Int]
    myList.insert(1)
    myList.insert(2)
    myList.insert(3)
    myList.insert(-1)
    myList.insert(5)
    myList.insert(-1)
    myList.insert(5)
    myList.insert(0)
    myList.insert(4)
    println("list after 9 inserts should be (-1,-1,0,1,2,3,4,5,5)")
    println("")
    println(myList.toString())
    println("")
    for (i <- 0 until 9) {
      myList.undoLastModification()
      println("undo number", i + 1)
      println("")
      println(myList.toString())
      println("")
    }
    println("list after 9 undo operations should be Empty")
    assert(myList.isEmpty)
    println("")
    println(myList.toString())
    println("")
    myList.insert(1)
    myList.insert(2)
    myList.insert(3)
    myList.insert(-1)
    myList.insert(5)
    myList.insert(-1)
    myList.insert(5)
    myList.insert(0)
    myList.insert(4)
    println("list after 9 inserts should be (-1,-1,0,1,2,3,4,5,5)")
    println("")
    println(myList.toString())
    println("")
    myList.remove(1)
    println("list after removing 1 should be (-1,-1,0,2,3,4,5,5)")
    println("")
    assert(myList.length == 8)
    println(myList.toString())
    println("")
    myList.remove(-1)
    println("list after removing -1 should be (0,2,3,4,5,5")
    println("")
    assert(myList.length == 6)
    assert(myList.head == 0)
    println(myList.toString())
    println("")
    myList.remove(5)
    println("list after removing 5 should be (0,2,3,4)")
    println("")
    assert(myList.length == 4)
    println(myList.toString())
    println("")
    val jobQueue = new LectureQueue[(String,Int)]
    jobQueue.enqueue("insert",1)
    jobQueue.enqueue("insert",2)
    jobQueue.enqueue("remove", 4)
    jobQueue.enqueue("remove", 0)
    jobQueue.enqueue("remove", 2)
    jobQueue.enqueue("remove", 3)
    jobQueue.enqueue("remove", 1)
    myList.processBatch(jobQueue)
    println("list after calling processBatch should be empty")
    println("")
    assert(myList.isEmpty)
    println(myList.toString())
    println("")
    myList.undoLastModification()
    println("list after calling undoLastModification should be (0,2,3,4)")
    println(myList.toString())
    println("")
    assert(myList.head == 0)
    assert(myList.length == 4)
    println(myList.toString())
    println("")
  }

  behavior of "simple inserting"
  it should "change head and stuff" in {
    val myList = new SortedList[Int]
    myList.insert(3)
    assert(myList.length == 1)
    assert(myList.head == 3)
    myList.insert(2)
    assert(myList.head == 2)
    assert(myList.length == 2)
    myList.insert(4)
    println(myList.toString())
    myList.insert(1)
    println(myList.toString())
    //assert(!myList.remove(400))
    myList.insert(1)
    println(myList.toString())
    myList.insert(3)
    println(myList.toString())
    myList.insert(4)
    println(myList.toString())
    myList.insert(0)
    println(myList.toString())
    myList.insert(0)
    println(myList.toString())
    myList.insert(7)
    println(myList.toString())
    myList.insert(4)
    println(myList.toString())
    myList.insert(7)
    println(myList.toString())
    myList.insert(1)
    println(myList.toString())
    myList.insert(0)
    println(myList.toString())
    myList.insert(7)
    println(myList.toString())
    myList.insert(7)
    println(myList.toString())
    myList.insert(5)
    println(myList.toString())
    myList.insert(6)
    println(myList.toString())
    myList.insert(5)
    println(myList.toString())
    myList.insert(6)
    println(myList.toString())
    myList.insert(4)
    println(myList.toString())
    myList.insert(-10)
    println(myList.toString())
    myList.insert(-22)
    println(myList.toString())
    myList.insert(-10)
    println(myList.toString())
    myList.insert(0)
    println(myList.toString())
    myList.insert(8)
    println(myList.toString())
    myList.insert(-23)
    println(myList.toString())
    myList.insert(-22)
    println(myList.toString())
    myList.insert(-22)
    println(myList.toString())
    myList.insert(9)
    myList.insert(9)
    println(myList.toString())
    myList.insert(-23)
    println(myList.toString())
    myList.insert(-34)
    println(myList.toString())
    myList.insert(-4)
    println(myList.toString())
    myList.insert(-15)
    println(myList.toString())
    assert(myList.head == -34)
    assert(myList.length == 35)
    assert(myList(33) == 9)
    //1
    myList.remove(0)
    assert(myList.length == 31)
    println(myList.toString())
    //2
    myList.remove(2)
    assert(myList.length == 30)
    //3
    myList.remove(-34)
    assert(myList.length == 29)
    assert(myList.head == -23)
    println(myList.toString())
    //4
    myList.remove(9)
    assert(myList.length == 27)
    //5
    myList.remove(-23)
    assert(myList.length == 25)
    assert(myList.head == -22)
    println("just before the undoing")
    println(myList.toString())
    println("undoing this stuff")
    println(" ")
    myList.undoLastModification()
    println(" ")
    println("list after first  undo")
    println(myList.toString())
    myList.undoLastModification()
    println("list after second undo")
    println(" ")
    println(myList.toString())
    myList.undoLastModification()
    println("list after third undo")
    println(" ")
    println(myList.toString())
    myList.undoLastModification()
    println("list after fourth undo")
    println(" ")
    println(myList.toString())
    myList.undoLastModification()
    println("list after fifth undo")
    println(" ")
    println(myList.toString())
    myList.undoLastModification()
    println("list after sixth undo")
    println(" ")
    println(myList.toString())
    myList.undoLastModification()
    println("list after seventh undo")
    println(" ")
    println(myList.toString())
  }

  behavior of "the methods"
  it should "work" in {
    val myList = new SortedList[Int]
    assertThrows[IllegalArgumentException]{myList.undoLastModification()}
    myList.insert(1)
    myList.undoLastModification()
    assert(myList.length == 0)
    assertThrows[IllegalArgumentException]{myList.undoLastModification()}
    myList.insert(0)
    myList.insert(1)
    myList.insert(2)
    myList.undoLastModification()
    assert(myList.length == 2)
    myList.undoLastModification()
    assert(myList.length == 1)
    myList.undoLastModification()
    assert(myList.length == 0)
    for (i <- 0 to 10) {
      val valToInsert = i
      myList.insert(valToInsert)
      assert(myList.length == (i + 1))
      assert(myList(i) == i)
      assert(myList.apply(i) == i)
      assertThrows[IllegalArgumentException]{myList(13)}
    }
    assert(!myList.remove(20))
    var curLen = myList.length
    for (i <- 0 to 10) {
      println(myList.toString())
      assert(myList.remove(i))
      assert(!myList.remove(i))
      curLen -= 1
      assert(myList.length == curLen)
      if (i < 10) assert(myList.head == (i + 1))
      if (i == 10) assert(myList.length == 0)
    }
    for (i <- 0 until 10) {
      myList.insert(2)
      assert(myList.length == (i + 1))
    }
    myList.insert(3)
    assert(myList.length == 11)
    assert(myList.head == 2)
    myList.remove(2)
    assert(myList.length == 1)
    assert(myList.head == 3)
    assert(!myList.remove(2))
    assert(myList.remove(3))
    myList.undoLastModification()
    assert(myList.length == 1)
    assert(myList.remove(3))
    assert(myList.length == 0)
    assert(!myList.remove(3))
    myList.insert(1)
    myList.insert(2)
    for (i <- 0 until 5) myList.insert(3)
    myList.insert(4)
    assert(myList.length == 8)
    println(myList.toString())
    myList.remove(3)
    println(myList.toString())
    assert(myList.length == 3)
    myList.undoLastModification()
    println(myList.toString())
    assert(myList.length == 8)
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
    jobQueue.enqueue("remove",0)
    myList.processBatch(jobQueue)
    assert(myList.length == 0)
  }

  behavior of "processBatch"
  it should "process this " in {
    val myList = new SortedList[Int]
    val jobQueue = new LectureQueue[(String, Int)]
    for (i <- 0 until 10) jobQueue.enqueue("insert", i)
    for (i <- 0 until 10) jobQueue.enqueue("remove", i)
    myList.processBatch(jobQueue)
    assert(myList.length == 0)
  }

  behavior of "advancedProcessBatch"
  it should "process this" in {
    val myList = new SortedList[Int]
    val jobQueue = new LectureQueue[(String, Int)]
    for (i <- 0 until 10) jobQueue.enqueue("insert", i)
    println("list should be 0-9")
    println("")
    println(myList.toString())
    println("")
    jobQueue.enqueue("remove", 4)
    jobQueue.enqueue("insert", 1)
    jobQueue.enqueue("insert", 1)
    jobQueue.enqueue("insert", 2)
    jobQueue.enqueue("insert", 2)
    jobQueue.enqueue("remove", 1)
    jobQueue.enqueue("remove", 2)
    myList.processBatch(jobQueue)
    assert(myList.length == 7)
    assert(myList(0) == 0)
    assert(myList(1) == 3)
    assert(myList(2) == 5)
    assert(myList(3) == 6)
    assert(myList(4) == 7)
    assert(myList(5) == 8)
    assert(myList(6) == 9)
  }

  behavior of "iterator"
  it should "contain the correct number of elements" in {
    val myList = new SortedList[Int]
    for (i <- 0 until 4) {
      myList.insert(i)
    }
    var counter = 0
    for (i <- myList.iterator) {
      counter += 1
    }
    println(myList.toString())
    assert(counter == 4)
  }

}

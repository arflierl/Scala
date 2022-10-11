/**
 * cse250.pa4.HashTableMapTests.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 */

package cse250.pa4

import org.scalatest.FlatSpec

class HashTableMapTests extends FlatSpec {
  val testSize = 10
  val inputKeys = Array.tabulate(testSize)(i => i + 1)
  val inputValues = Array.tabulate(testSize)(i => i.toString * i)
  behavior of "HashTableMap.insert"
  it should "add the (key,value) pairs" in {
    val hashMap = new HashTableMap[Int, String]
    val elements = inputKeys.zip(inputValues)
    for ((k, v) <- elements) {
      hashMap.addOne((k, v))
      //println(hashMap._bucketArray.toList)
      assert(hashMap.contains(k))
    }
    val iterator = hashMap.iterator
    val elementSet = collection.mutable.Set[(Int, String)]()
    for (_ <- elements.indices) {
      assert(iterator.hasNext)
      val elem = iterator.next
      elementSet.add(elem)
    }
    for (i <- elements.indices) {
      val elem = elements(i)
      assert(elementSet.contains(elem))
    }

    println("--------------------------------------------------")
    println("----------------printing iterator----------------")
    for (i <- hashMap.iterator) println(i)
    println("--------------------------------------------------")
    println("--------------------------------------------------")
    println("-------------adding custom insertions-------------")
    hashMap.addOne(100, "should pile on")
    println("--------------------------------------------------")
    println("added k = 100")
    println("--------------------------------------------------")
    hashMap.addOne(300, "should pile on")
    println("--------------------------------------------------")
    println("added k = 300")
    println("--------------------------------------------------")
    println("----------------printing iterator----------------")
    for (i <- hashMap.iterator) println(i)
    println("--------------------------------------------------")
    hashMap.addOne(932, "should rehash") //no it is _N == 40
    println("--------------------------------------------------")
    println("added k = 932")
    println("--------------------------------------------------")
    assert(hashMap._n == 13)
    assert(hashMap._N == 40)
    println("--------------------------------------------------")
    println("----------------printing iterator----------------")
    for (i <- hashMap.iterator) println(i)
    println("--------------------------------------------------")
    println("--------------------------------------------------")
    hashMap.addOne(60, "2nd customs")
    hashMap.addOne(180, "2nd customs")
    assert(hashMap._n == 15)
    assert(hashMap._N == 40)
    hashMap.addOne(260, "2nd customs")
    hashMap.addOne(33, "2nd customs")
    hashMap.addOne(73, "2nd customs")
    hashMap.addOne(113, "2nd customs")
    assert(hashMap._n == 19)
    assert(hashMap._N == 40)
    println("--------------------------------------------------")
    println("------adding 3 to index 20 and 3 to index 33------")
    println("--------------------------------------------------")
    println("----------------printing iterator----------------")
    for (i <- hashMap.iterator) println(i)
    println("--------------------------------------------------")
    println("--------------------------------------------------")
    for ((k, v) <- elements) hashMap.removeOne(k)
    hashMap.removeOne(100)
    hashMap.removeOne(300)
    hashMap.removeOne(932)
    hashMap.removeOne(60)
    hashMap.removeOne(180)
    hashMap.removeOne(260)
    hashMap.removeOne(33)
    hashMap.removeOne(73)
    hashMap.removeOne(113)
    for (i <- hashMap._bucketArray) {
      assert(i.isEmpty)
    }
    assert(hashMap._n == 0)
    println("--------------------------------------------------")
    println("------------- printing empty iterator-------------")
    for (i <- hashMap.iterator) println(i)
    println("--------------------------------------------------")
  }

  behavior of "my test"
  it should "work" in {
    val hashMap = new HashTableMap[Int, String]
    val s = "entry: "
    for (i <- 0 to 94) {
      val x = s + i.toString
      //println("before", hashMap._n)
      //println("i", i)
      hashMap.addOne((i, x))
      //println("after", hashMap._n)
    }
    assert(hashMap._n == 95)
    for (i <- 0 to 10) hashMap.addOne(i, "replaced")
    assert(hashMap._n == 95)
    for (i <- 0 to 94) {
      assert(hashMap.contains(i))
    }
    for (i <- hashMap.iterator) println(i)
    println(hashMap._N)
  }
}


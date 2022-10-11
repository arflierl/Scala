/**
 * cse250.pa3.TrieTests.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT:
 * Person#:
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa3

import org.scalatest._


class TrieTests extends FlatSpec with BeforeAndAfter {

  behavior of "Trie"
  it should "store the words from the assignment" in {
    val testTrie = new Trie
    val words = List("a", "i", "an", "in", "to", "and", "inn", "tea", "ted", "ten")
    val counts = List(15, 11, 22, 5, 7, 17, 9, 3, 4, 12)
    for ((word, count) <- words zip counts; _ <- 0 until count) {
      testTrie.insert(word)
    }

    for ((word, count) <- words zip counts; _ <- 0 until count) {
      assert(testTrie.count(word) == count)
    }
  }

  it should "pass this easy test" in {
    val testTrie = new Trie
    val words = List("", "a", "and", "baseball", "base", "foot", "football")
    for (i <- words) testTrie.insert(i)
    for (i <- 0 until 3) {
      testTrie.insert("football")
      testTrie.insert("nasty")
    }
    testTrie.insert("")
    for (i <- 0 until 88) testTrie.insert("mostInserted")
    for (i <- testTrie.iterator) println(i)
    println(testTrie.mostCommon(3).toString())
    println(testTrie.allWordsOfLength(8).toString())
    println(testTrie.allWordsOfLength(12).toString())
    println(testTrie.mostCommonWithPrefix("foo", 2).toString())
    println(testTrie.mostCommonWithPrefix("bas", 2).toString())
    println(testTrie.mostCommonWithPrefix("bas", 6).toString())
    println(testTrie.mostCommonWithPrefix("foo", 6).toString())
    println(testTrie.mostCommonWithPrefix("bas", 1).toString())
    println(testTrie.mostCommonWithPrefix("foo", 1).toString())
    for (i <- 0 until 88) {
      testTrie.insert("middle")
      testTrie.insert("low")
    }
    println(testTrie.mostCommon(3).toString())
    for (i <- 0 until 88) testTrie.insert("high")
    println(testTrie.mostCommon(3).toString())
    testTrie.insert("low")
    testTrie.insert("low")
    testTrie.insert("high")
    println(testTrie.mostCommon(3).toString())

  }


}


/**
 * TaxEntryProcessor.scala
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
 * UBIT:
 */
package cse250.pa0.objects

import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter

import cse250.objects.TaxParcel

import scala.io.Source

object AssessmentDataProcessor {

  //helper functions idea from Piazza post @69
  def getLines(filename: String): scala.collection.mutable.ArrayBuffer[String] = {
    val inputFile = scala.io.Source.fromFile(filename)
    val lines = inputFile.getLines()
    //reference - https://alvinalexander.com/source-code/how-to-create-scala-arraybuffer-syntax
    val entries = scala.collection.mutable.ArrayBuffer[String]()
    for (line <- lines) entries += line
    inputFile.close()
    entries
  }

  def removeColumns(dirty: scala.collection.mutable.ArrayBuffer[String]): Array[String] = {
    val filter = Seq[Int]( 8, 9, 10, 11, 12, 13, 20, 21, 22, 23, 24, 31, 32, 33, 39, 40, 41)
    var clean = scala.collection.mutable.ArrayBuffer[String]()
    var row:Int = -1
    for (line <- dirty) {
      row += 1
      var commaCounter:Int = 0
      var inQuotes:Boolean = false
      var acceptableProperty:Boolean = true
      var enter: String = ""
      var zipChars:Int = 0

      for (char <- line) {
        if (char == '"') {
          if (inQuotes == false) {
            inQuotes = true
          }else inQuotes = false
        }

        if (char == ',') {
          if (inQuotes == false) commaCounter += 1
        }

        if (commaCounter == 19) zipChars = zipChars + 1

        //reference - https://docs.scala-lang.org/overviews/collections/sets.html
        //reference - https://www.scala-lang.org/api/2.13.1/scala/collection/Seq.html
        if (!filter.contains(commaCounter)) {
          enter = enter + char
        }
      }

      if (zipChars < 2) acceptableProperty = false
      if (acceptableProperty == true) clean += enter

    }
    clean.toArray
  }

  def sanitizeData(filename: String): Unit = {

    val outputFile = new BufferedWriter(new FileWriter( new File(filename + "-updated")))

    val entries:Array[String] = removeColumns(getLines(filename))

    for (i <- entries) {
      outputFile.write(i)
      outputFile.write('\n')
    }

    outputFile.close()
  }

  def computeMostExpensiveEntry(filename: String): TaxParcel = {

    val t = new TaxParcel
    val entries:Array[String] = getLines(filename).toArray[String]

    //get the prices
    val prices = scala.collection.mutable.ArrayBuffer[Int]()
    var row:Int = -1
    for (line <- entries) {
      row += 1
      var commaCounter: Int = 0
      var inQuotes: Boolean = false
      var priceEntry: Boolean = false
      var price: String = ""

      for (char <- line) {

        if (char == '"') {
          if (inQuotes == false) {
            inQuotes = true
          } else inQuotes = false
        }

        if (char == ',') {
          if (inQuotes == false) {
            if (commaCounter == 15) {
              priceEntry = false
              if (row > 0) prices += price.toInt

            }
            commaCounter += 1
          }
        }

        if (commaCounter == 15 && char != ',') {
          priceEntry = true
          price = price + char
        }
      }



    }


    val priceArray = prices.toArray
    val expensiveIndex = priceArray.indexOf(priceArray.max) + 1
    val expensiveColumns = entries(expensiveIndex).mkString
    val expensiveData = scala.collection.mutable.ArrayBuffer[String]()
    var enter:String = "" //what I will put into the arrayBuffer elements
    var comma:Int = 0
    var inQuotes:Boolean = false

    for (char <- expensiveColumns) {

      if (char == '"') {
        if (inQuotes == true) {
          inQuotes = false
        }else {
          inQuotes = true

        }
      }

      if (char != ',' && char != '"') {
        enter += char
      }

      if (char == ',' && inQuotes == false) {
        expensiveData += enter
        comma += 1
        enter = ""
      }

    }
    expensiveData += enter
    val finalData = expensiveData.toArray

    for (i <- 0 until 29) {
      t.parcelInfo += ((TaxParcel.HEADERS(i), finalData(i)))
    }

    t
  }

  def countPriceRange(filename: String, lower: Int, upper: Int): Int = {
    0
  }
}

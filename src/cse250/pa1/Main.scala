/**
 * Main.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Modify at your leisure, but this will not be graded.
 */
package cse250.pa1

import cse250.objects.TaxEntry

import scala.collection.mutable

object Main {
  def loadEntries(filename: String, numToLoad: Int) = {
    // Scala Cookbook reading CSV:
    // https://www.oreilly.com/library/view/scala-cookbook/9781449340292/ch12s06.html
    val bufferedSource = io.Source.fromFile(filename)
    val lines = bufferedSource.getLines
    val buffer = new mutable.ArrayBuffer[TaxEntry]()
    lines.next
    for (_ <- 1 to numToLoad) {
      val line = lines.next
      val cols = line.split(',')
      var colsIndex = 0
      val taxEntry = new TaxEntry
      for (headerIndex <- 0 until TaxEntry.HEADERS.length) {
        val entry = {
          if (colsIndex < cols.length && cols(colsIndex).length != 0 && cols(colsIndex)(0) == '"') {
            val sb = new StringBuilder
            sb ++= cols(colsIndex)
            sb += ','
            while (cols(colsIndex).length == 0 || cols(colsIndex).last != '"') {
              colsIndex += 1
              sb ++= cols(colsIndex)
              sb += ','
            }
            (TaxEntry.HEADERS(headerIndex), sb.result.dropRight(1))
          }
          else if (colsIndex < cols.length) {
            (TaxEntry.HEADERS(headerIndex), cols(colsIndex))
          }
          else {
            (TaxEntry.HEADERS(headerIndex), "")
          }
        }
        colsIndex += 1
        taxEntry.infoMap.addOne(entry)
      }
      buffer.append(taxEntry)
    }
    bufferedSource.close
    buffer
  }

  def main(args: Array[String]): Unit = {
    val taxEntryStore = new DataEntryStore[TaxEntry](10)
    val filename = "data/2017-2018_Assessment_Roll-updated-small.csv"

    var numLines = 5
    for (entry <- loadEntries(filename, numLines)) {
      taxEntryStore.insert(entry)
    }
    println(s"Storage after $numLines additions:")
    println("-----")
    println(taxEntryStore)
    println("-----")

    numLines = 10
    for (entry <- loadEntries(filename, numLines)) {
      taxEntryStore.insert(entry)
    }
    println(s"Storage after $numLines more additions:")
    println("-----")
    println(taxEntryStore)
    println("-----")

    numLines = 5
    for (entry <- loadEntries(filename, numLines)) {
      taxEntryStore.remove(entry)
    }
    println(s"Storage after removal of first $numLines lines:")
    println("-----")
    println(taxEntryStore)
    println("-----")

    numLines = 10
    for (entry <- loadEntries(filename, numLines)) {
      taxEntryStore.remove(entry)
    }
    println(s"Storage after removal of first $numLines lines:")
    println("-----")
    println(taxEntryStore)
    println("-----")

    numLines = 1
    for (entry <- loadEntries(filename, numLines); _ <- 1 to 5) {
      taxEntryStore.insert(entry)
    }
    println(s"Storage after adding 5 copies of of first line:")
    println("-----")
    println(taxEntryStore)
    println("-----")

    numLines = 1
    for (entry <- loadEntries(filename, numLines)) {
      taxEntryStore.remove(entry)
    }
    println(s"Storage after removing first line:")
    println("-----")
    println(taxEntryStore)
    println("-----")


  }
}

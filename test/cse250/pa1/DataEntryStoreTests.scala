package cse250.pa1

import cse250.objects.TaxEntry
import org.scalatest.{BeforeAndAfter, FlatSpec}

import scala.collection.mutable

class DataEntryStoreTests extends FlatSpec with BeforeAndAfter {

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

  val smallFilename = "data/2017-2018_Assessment_Roll-updated-small.csv"
  val largeFilename = "data/2017-2018_Assessment_Roll-updated.csv"
  val maxCapacity = 10
  var dataStore: DataEntryStore[TaxEntry] = _


  before {
    dataStore = new DataEntryStore[TaxEntry](maxCapacity)
  }

  behavior of "DataEntryStore.length"
  it should "be 0 when initialized" in {
    assert(dataStore.length == 0)
  }

  it should "be updated after each insertion" in {
    val entries = loadEntries(smallFilename, maxCapacity)
    for (i <- 0 until entries.length) {
      dataStore.insert(entries(i))
      assert(dataStore.length == i + 1)
    }
  }

  behavior of "DataEntryStore.insert"
  it should "store every element, as long as capacity isn't full, with newer elements at the end" in {
    val entries = loadEntries(smallFilename, maxCapacity)
    for (i <- 0 until entries.length) {
      dataStore.insert(entries(i))
      assert(dataStore.length == i + 1)
      var itemIndex = 0
      for (entry <- dataStore.iterator) {
        assert(entry == entries(itemIndex))
        itemIndex += 1
      }
    }
  }

  it should "store duplicate entries" in {
    val entries = loadEntries(smallFilename, 1)
    for (i <- 0 until maxCapacity) {
      dataStore.insert(entries(0))
      assert(dataStore.length == i + 1)
    }
    for (entry <- dataStore.iterator) assert(entry == entries(0))
  }

  it should "overwrite the oldest entry when capacity is full" in {
    val entries = loadEntries(smallFilename, maxCapacity + 5)
    for (i <- 0 until maxCapacity + 5) {
      val entry = entries(i)
      dataStore.insert(entry)
      if (i < maxCapacity) {
        assert(dataStore.length == i + 1)
      }
      else {
        assert(dataStore.length == maxCapacity)
      }
    }

    val iter = dataStore.iterator
    for (i <- 0 until maxCapacity) {
      assert(iter.hasNext)
      assert(iter.next == entries(i + 5))
    }
  }

  behavior of "DataEntryStore.remove"
  it should "report the correct return value based on removal" in {
    val entries = loadEntries(smallFilename, maxCapacity)
    for (i <- 0 until entries.length) {
      dataStore.insert(entries(i))
    }
    for (i <- 0 until maxCapacity if i % 2 == 0) {
      assert(dataStore.remove(entries(i)))
      assert(dataStore.length == maxCapacity - (i / 2 + 1))
      assert(!dataStore.remove(entries(i)))
      assert(dataStore.length == maxCapacity - (i / 2 + 1))
    }
    for (i <- 0 until maxCapacity) {
      if (i % 2 != 0) {
        assert(dataStore.remove(entries(i)))
        assert(dataStore.length == maxCapacity/2 - ((i-1) / 2 + 1))
      }
    }
  }

  it should "remove duplicate entries" in {
    val entries = loadEntries(smallFilename, maxCapacity+1)
    for (i <- 0 until maxCapacity) {
      dataStore.insert(entries(0))
      assert(dataStore.length == i + 1)
    }
    for (entry <- dataStore.iterator) assert(entry == entries(0))

    assert(dataStore.remove(entries(0)))
    assert(dataStore.length == 0)
    assert(!dataStore.iterator.hasNext)

    for (i <- 0 until maxCapacity) {
      dataStore.insert(entries(i))
      assert(dataStore.length == i + 1)
    }
    var entryIndex = 0
    for (entry <- dataStore.iterator) {
      assert(entry == entries(entryIndex))
      entryIndex += 1
    }

    for (_ <- 0 until maxCapacity) {
      dataStore.insert(entries(maxCapacity))
      assert(dataStore.length == maxCapacity)
    }
    for (entry <- dataStore.iterator) assert(entry == entries(maxCapacity))
    for (_ <- 0 until maxCapacity/2) {
      dataStore.insert(entries(0))
      assert(dataStore.length == maxCapacity)
    }
    entryIndex = 0
    for (entry <- dataStore.iterator) {
      if (entryIndex < maxCapacity - (maxCapacity/2)) {
        assert(entry == entries(maxCapacity))
      }
      else {
        assert(entry == entries(0))
      }
      entryIndex += 1
    }

    assert(dataStore.remove(entries(maxCapacity)))
    assert(dataStore.length == maxCapacity/2)
    for (entry <- dataStore.iterator) assert(entry == entries(0))
    assert(dataStore.remove(entries(0)))
    assert(dataStore.length == 0)
    assert(!dataStore.iterator.hasNext)
  }
}

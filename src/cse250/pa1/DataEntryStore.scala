/**
 * DataEntryStore.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT: edgarust
 * Person#: 50230866
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT: edgarust
 */
package cse250.pa1

import cse250.objects.EmbeddedListNode
import scala.util.control.Breaks._

class DataEntryStore[A >: Null <: AnyRef](private val capacity: Int = 100)
  extends collection.mutable.Seq[A] {
  private val dataArray = Array.fill[EmbeddedListNode[A]](capacity)(new EmbeddedListNode[A])
  private var headIndex = -1
  private var tailIndex = -1
  private var numStored = 0

  /** Inserts element to tail of list. */
  def insert(elem: A): Unit = {
    //Empty List
    if(numStored == 0){
      //Create First Element
      numStored += 1
      headIndex = 0
      tailIndex = 0
      dataArray.head.value = elem
    }
    //Not-Full List
    else if(0 < numStored && numStored < capacity){
      var editNode: EmbeddedListNode[A] = null
      var indX = 0
      //Find first empty node
      breakable(
        for(node <- dataArray){
          if(node.value == null){
            editNode = node
            break()
          }
          indX += 1
        }
      )
      numStored += 1
      //Set done to elem
      editNode.value = elem
      editNode.prev = tailIndex
      dataArray(tailIndex).next = indX
      tailIndex = indX
    }
    //Full List
    else if(numStored == capacity){
      //Replace the value
      dataArray(headIndex).value = elem
      //Double-Link
//      println("head.prev : " + dataArray(headIndex).prev + " --> " + tailIndex)
      dataArray(headIndex).prev = tailIndex
//      println("tail.next : " + dataArray(tailIndex).next + " --> " + headIndex)
      dataArray(tailIndex).next = headIndex
      //Next node in list becomes head
//      println("next.prev : " + dataArray(dataArray(headIndex).next).prev + " --> " + "-1")
      dataArray(dataArray(headIndex).next).prev = -1
      //Head becomes new Tail
//      println("     tail : " + tailIndex + " --> " + headIndex)
      tailIndex = headIndex
      //Next elem is the new head
//      println("     head : " + headIndex + " --> " + dataArray(headIndex).next)
      headIndex = dataArray(headIndex).next
    }
    /*
    println("============================================")
    println("numStored : " + numStored)
    println(" Capacity : " + capacity)
    println("     Head : " + headIndex)
    println("     Tail : " + tailIndex)
    if (numStored == capacity) {
      println("FULL")
    }
    for(indX <- dataArray.indices){
      val node = dataArray(indX)
      print("indX : " + indX + ", ")
      print("prev : " + node.prev + ", ")
      print("next : " + node.next + ", ")
      println("value : " + node.value)
    }
    println("============================================")
    */
  }

  /** Removes all copies of the given element. */
  def remove(elem: A): Boolean = {
    var exists: Boolean = false
    //Iterate until relative tail
    for(node <- dataArray){
//      print(node.prev + ", " + node.next)
      //If exists
      if(node.value == elem){
        exists = true
        node.value = null
        numStored -= 1
        if(numStored > 0){
          //If at relative head
          if(node.prev == -1 && node.next != -1){
            //Update headIndex
            headIndex = node.next
            dataArray(node.next).prev = -1
            node.next = -1
          }
          //If at relative tail
          if(node.prev != -1 && node.next == -1){
            //Update tailIndex
            tailIndex = node.prev
            dataArray(node.prev).next = -1
            node.prev = -1
          }
          //Only 1 node
          if(node.prev == -1 && node.next == -1){
            node.prev = -1
            node.next = -1
            headIndex = -1
            tailIndex = -1
          }
          if(node.prev != -1 && node.next != -1){
            dataArray(node.prev).next = node.next
            dataArray(node.next).prev = node.prev
            node.prev = -1
            node.next = -1
          }
        }
      }
    }
    exists
  }

  /** Returns the count of nodes containing given entry. */
  def countEntry(entry: A): Int = {
    var entries: Int = 0
    for(node <- dataArray){
      if(node.value == entry){
        entries += 1
      }
    }
    entries
  }

  /** Gets the element at the specified index. */
  override def apply(indX: Int): A = {
    dataArray(indX).value
  }

  /** Replaces element at given index with a new value. */
  override def update(indX: Int, elem: A): Unit = {
    dataArray(indX).value = elem
  }

  /** Returns an Iterator that can be used only once. */
  def iterator: Iterator[A] = new Iterator[A] {
    private var current = headIndex

    override def hasNext: Boolean = current != -1

    override def next(): A = {
      val prev = current
      current = dataArray(current).next
      dataArray(prev).value
    }
  }

  /** Returns the length of the stored list. */
  override def length: Int = numStored

  override def toString: String = if (numStored == 0) "" else this.iterator.addString(new StringBuilder, "\n").result()
}

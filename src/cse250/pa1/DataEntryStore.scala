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
 * UBIT:
 * Person#:
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
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
      var indX: Int = 0
      while(indX < capacity){
        if(dataArray(indX).value == null){
          numStored += 1
          //Break out of loop using the iterator
          indX = capacity
          dataArray(indX).value = elem
          //Append element at the end of array
          dataArray(indX).prev = tailIndex
          //Make sure the other end is pointing at the new tail
          dataArray(tailIndex).next = indX
        }
        else{
          indX += 1
        }
      }
    }
    //Full List
    else{
      //Replaces the value of the head
      dataArray(headIndex).value = elem
      //Link the new tail to the previous tail
      dataArray(headIndex).prev = tailIndex
      //New Tail
      dataArray(headIndex).next = -1
      //Old head becomes New tail
      tailIndex = headIndex
      //Next item is the head
      val tempIndX: Int = headIndex
      headIndex = dataArray(headIndex).next
      //Sets previous as head
      dataArray(tempIndX).prev = -1
    }
  }

  /** Removes all copies of the given element. */
  def remove(elem: A): Boolean = {
    var exists: Boolean = false
    for(nodes <- dataArray){
      if(nodes.value == elem){
        exists = true
        if(nodes.prev != -1){
          dataArray(nodes.prev).next = nodes.next
        }
        if(nodes.next != -1){
          dataArray(nodes.next).prev = nodes.prev
        }
        nodes.value = null
        nodes.prev = -1
        nodes.next = -1
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

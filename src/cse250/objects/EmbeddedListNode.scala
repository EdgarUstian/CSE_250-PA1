/**
 * cse250.objects.EmbeddedListNode.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * DO NOT MODIFY THIS FILE
 */
package cse250.objects

class EmbeddedListNode[A >: Null <: AnyRef](var value: A = null, var prev: Int = -1, var next: Int = -1) {

}

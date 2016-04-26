package com.art4ul.algorithms.sort

/**
  * Created by artsemsemianenka on 3/20/16.
  */

class HeapSort[T <% Ordered[T]](array: Array[T]) {

  var heapTail: Int = array.size - 1

  val leftIdx: Int => Int = x => 2 * x + 1
  val rightIdx: Int => Int = x => 2 * x + 2
  val parentIdx: Int => Int = x => x / 2
  val outOfScope: Int => Boolean = x => x > heapTail

  def getElement(indexResolver: Int => Int)(index: Int): Option[T] = index match {
    case i if outOfScope(i) || outOfScope(indexResolver(i)) => None
    case i => Some(array(indexResolver(i)))
  }

  def getLeft(index: Int) = getElement(leftIdx)(index)

  def getRight(index: Int) = getElement(rightIdx)(index)

  def getParent(index: Int) = getElement(parentIdx)(index)

  def swap(indexResolver: Int => Int)(index: Int) = {
    val from = index
    val to = indexResolver(index)
    val tmp = array(from)
    array(from) = array(to)
    array(to) = tmp
  }

  def swapLeft(index: Int) = swap(leftIdx)(index)

  def swapRight(index: Int) = swap(rightIdx)(index)

  def maxHeapify(index: Int): Unit = (getLeft(index), getRight(index)) match {
    case (Some(left), Some(right)) if (left >= right && left > array(index)) =>
      swapLeft(index)
      maxHeapify(leftIdx(index))

    case (Some(left), Some(right)) if (left <= right && right > array(index)) =>
      swapRight(index)
      maxHeapify(rightIdx(index))

    case (Some(left), None) if left > array(index) =>
      swapLeft(index)

    case _ => // Do Nothing
  }

  def popHead: T = {
    val result = array(0)
    array(0) = array(heapTail)
    heapTail -= 1
    maxHeapify(0)
    result
  }

  def sort(): Unit = {
    for (i <- (0 to ((heapTail) / 2)).reverse) {
      maxHeapify(i)
    }
    while (heapTail > 0) {
      val insertPosition = heapTail
      val headHeap = popHead
      array(insertPosition) = headHeap
    }
  }
}

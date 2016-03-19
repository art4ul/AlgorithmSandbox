package com.art4ul.algorithms.sort

import scala.annotation.tailrec

/**
  * Created by artsemsemianenka on 3/19/16.
  */
class MergeSort[T <% Ordered[T]] {

  def merge(left: List[T], right: List[T]): List[T] ={
    @tailrec
    def inner(left: List[T], right: List[T], result: List[T]): List[T] = (left, right) match {
      case (leftHead :: leftTail, rightResult@(rightHead :: _)) if (leftHead <= rightHead) => inner(leftTail, rightResult, result :+ leftHead)
      case (leftResult@(leftHead :: _), rightHead :: rightTail) if (leftHead > rightHead) => inner(leftResult, rightTail, result :+ rightHead)

      case (Nil, rightResult@(head :: _)) => result ++ rightResult
      case (leftResult@(head :: _), Nil) => result ++ leftResult
    }
    inner(left, right, Nil)
  }

  def sort(array: List[T]): List[T] = array.size match {
    case size if size <= 1 => array
    case _ =>
      val mid = array.size / 2
      val leftSlice = array.slice(0, mid)
      val rightSlice = array.slice(mid, array.size)
      val sortedLeft = sort(leftSlice)
      val sortedRight = sort(rightSlice)
      merge(sortedLeft,sortedRight)
  }

}


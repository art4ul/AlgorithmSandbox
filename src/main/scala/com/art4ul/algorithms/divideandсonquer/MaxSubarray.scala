package com.art4ul.algorithms.divideandсonquer

import scala.annotation.tailrec

/**
  * Created by artsemsemianenka on 3/17/16.
  */
class MaxSubArray(array: Array[Int]) {

  case class State(val max: Int, val sum: Long = Long.MinValue)

  val RightDirection = 1
  val LeftDirection = -1

  private def calcMaxSum(start: Int, target: Int, inc: Int = RightDirection)(initState: State): State = {
    @tailrec
    def inner(position: Int)(state: State, acc: Int = 0): State = position match {
      case pos if (pos == target + inc) => state
      case _ =>
        val newAcc = acc + array(position)
        val newState = if (newAcc > state.sum) State(position, newAcc) else state
        inner(position + inc)(newState, newAcc)
    }

    inner(start)(initState)
  }

  protected def findMaxCrossingSubArray(low: Int, mid: Int, high: Int): (Int, Int, Long) = {
    val leftState = calcMaxSum(mid, low, LeftDirection)(State(mid, Long.MinValue))
    val rightState = calcMaxSum(mid + 1, high, RightDirection)(State(mid + 1, Long.MinValue))
    (leftState.max, rightState.max, leftState.sum + rightState.sum)
  }

  def findMaxSubArray(low: Int = 0, high: Int = array.size - 1): (Int, Int, Long) = (low, high) match {
    case (low, high) if (low == high) => (low, high, array(low))

    case _ =>
      val mid = (low + high) / 2
      val leftSubarray@(_, _, leftSum) = findMaxSubArray(low, mid)
      val rightSubarray@(_, _, rightSum) = findMaxSubArray(mid + 1, high)
      val crossSubarray@(_, _, crossSum) = findMaxCrossingSubArray(low, mid, high)

      (leftSum, crossSum, rightSum) match {
        case v if (leftSum >= rightSum) && (leftSum >= crossSum) => leftSubarray
        case v if (rightSum >= leftSum) && (rightSum >= crossSum) => rightSubarray
        case _ => crossSubarray
      }

  }
}


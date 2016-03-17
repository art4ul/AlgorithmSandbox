package com.art4ul.algorithms.divideandсonquer

import scala.annotation.tailrec

/**
  * Created by artsemsemianenka on 3/17/16.
  */
class MaxSubArray(array: Array[Int]) {

  case class State(val max: Int, val sum: Long = Long.MinValue)

  @tailrec
  private def calcMaxSum(position: Int, target: Int, inc: Int = 1, state: State)(acc: Int = 0): State =
    if (position == target + inc) {
      state
    } else {
      val newAcc = acc + array(position)
      val newState = if (newAcc > state.sum) {
        State(position, newAcc)
      } else state
      calcMaxSum(position + inc, target, inc, newState)(newAcc)
    }


  protected def findMAxCrossingSubArray(low: Int, mid: Int, high: Int): (Int, Int, Long) = {
    val leftState = calcMaxSum(mid, low, -1, State(mid, Long.MinValue))(0)
    val rightState = calcMaxSum(mid + 1, high, 1, State(mid + 1, Long.MinValue))(0)
    (leftState.max, rightState.max, leftState.sum + rightState.sum)
  }

  def findMaxSubArray(low: Int = 0, high: Int = array.size - 1): (Int, Int, Long) = (low, high) match {
    case (low, high) if (low == high) => (low, high, array(low))

    case _ =>
      val mid = (low + high) / 2
      val leftSubarray@(_, _, leftSum) = findMaxSubArray(low, mid)
      val rightSubarray@(_, _, rightSum) = findMaxSubArray(mid + 1, high)
      val crossSubarray@(_, _, crossSum) = findMAxCrossingSubArray(low, mid, high)

      (leftSum, crossSum, rightSum) match {
        case v if (leftSum >= rightSum) && (leftSum >= crossSum) => leftSubarray
        case v if (rightSum >= leftSum) && (rightSum >= crossSum) => rightSubarray
        case _ => crossSubarray
      }

  }
}

object Test extends App {

  val array = Array(13, -3, -25, 20, -3, -16, -23, 18, 20, -7, 12, -5, -22, 15, -4, 7)

  val finder = new MaxSubArray(array)
  val result@(start,end, sum) = finder.findMaxSubArray()
  val subArray = array.slice(start,end+1)

  println(s"SubArray: ${subArray.mkString(",")}")
  println(s"Result:$result")
}


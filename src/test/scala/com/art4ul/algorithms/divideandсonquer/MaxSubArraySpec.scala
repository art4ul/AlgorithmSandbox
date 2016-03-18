package com.art4ul.algorithms.divideandсonquer

import org.scalatest.{FunSpec, Matchers}

/**
  * Created by artsemsemianenka on 3/18/16.
  */
class MaxSubArraySpec extends FunSpec with Matchers {

  describe("MaxSubArray Specification") {

    it("should find subArray") {
      val array = Array(13, -3, -25, 20, -3, -16, -23, 18, 20, -7, 12, -5, -22, 15, -4, 7)
      val expected = Array(18, 20, -7, 12)

      val finder = new MaxSubArray(array)
      val (start, end, sum) = finder.findMaxSubArray()
      val subArray = array.slice(start, end + 1)
      subArray shouldBe expected
      subArray.reduce(_+_) shouldBe sum
    }
  }


}

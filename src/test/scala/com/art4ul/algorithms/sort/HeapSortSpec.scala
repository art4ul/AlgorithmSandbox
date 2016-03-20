package com.art4ul.algorithms.sort

import org.scalatest.{FunSpec, Matchers}

/**
  * Created by artsemsemianenka on 3/20/16.
  */
class HeapSortSpec extends FunSpec with Matchers {

  describe("HeapSort Specification") {

    it("should sort array") {
      val testArray = Array(7, 1, 8, 9, 2, 10, 5, 4, 3, 6)
      /**
        *                 7 (0)
        *               /       \
        *            1(1)        8(2)
        *          /   \        /    \
        *       9(3)   2(4)   10(5)  5(6)
        *      / \     /
        *   4(7) 3(8) 6(9)
        *
        */


      val expected = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

      new HeapSort(testArray).sort()
      testArray shouldBe expected
    }
  }
}
package com.art4ul.algorithms.sort

import org.scalatest.{FunSpec, Matchers}

/**
  * Created by artsemsemianenka on 3/17/16.
  */
class InsertionSortSpec extends FunSpec with Matchers {

  describe("InsertionSort Specification") {

    it("should sort array") {
      val testArray = Array(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
      val expected = Array(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

      val sorter = new InsertionSort[Int]
      sorter.sort(testArray)

      testArray shouldBe expected
    }
  }

}

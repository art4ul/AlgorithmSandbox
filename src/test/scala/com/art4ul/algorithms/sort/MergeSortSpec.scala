package com.art4ul.algorithms.sort

import org.scalatest.{Matchers, FunSpec}

/**
  * Created by artsemsemianenka on 3/19/16.
  */
class MergeSortSpec extends FunSpec with Matchers {

  describe("MergeSort Specification") {

    it("should merge two sorted lists"){
      val a = List(1, 3, 5, 7, 8, 9)
      val b = List(2, 4, 8, 10)

      val sorter = new MergeSort[Int]
      sorter.merge(a, b) shouldBe List(1, 2, 3, 4, 5, 7, 8, 8, 9, 10)
    }

    it("should sort array") {
      val testArray = List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
      val expected = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

      val sorter = new MergeSort[Int]
      sorter.sort(testArray) shouldBe expected
    }
  }
}

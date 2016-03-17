package com.art4ul.collection.mutable

import org.scalatest.{FunSpec, Matchers}

class IntervalTreeSpec extends FunSpec with Matchers {

  describe("IntervalTree Specification") {

    it("should intersect all intervals") {
      {
        val tree = new IntervalTree[Int, String]
        tree.put(0 -> 100, "0 -> 100")
        tree.put(5 -> 8, "5 -> 8")
        tree.put(9 -> 15, "9 -> 15")
        tree.put(4 -> 11, "4 -> 11")
        tree.put(2 -> 4, "2 -> 4")
        tree.put(3 -> 5, "3-> 5")
        tree.find(3) shouldBe Set("0 -> 100", "2 -> 4", "3-> 5")
        tree.find(100) shouldBe Set("0 -> 100")
        tree.find(101) shouldBe Set()
        tree.find(5) shouldBe Set("0 -> 100","5 -> 8","4 -> 11","3-> 5")
      }


      {
        val tree = new IntervalTree[Int, String]
        tree.put(2 -> 3, "2 -> 3")
        tree.put(1 -> 4, "1 -> 4")
        tree.put(4 -> 5, "4 -> 5")
        tree.put(5 -> 10, "5 -> 10")
        tree.put(6 -> 9, "6 -> 9")
        tree.put(3 -> 5, "3-> 5")
        tree.find(9) shouldBe Set("5 -> 10", "6 -> 9")
        tree.find(-1) shouldBe Set()
      }

      {
        val tree = new IntervalTree[Int, String]
        tree.put(2 -> 5, "2 -> 5")
        tree.put(2 -> 3, "2 -> 3")
        tree.put(2 -> 3, "2 -> 3double")
        tree.put(2 -> 2, "2 -> 2")
        tree.put(3 -> 4, "3 -> 4")
        tree.put(5 -> 10, "5 -> 10")
        tree.put(6 -> 9, "6 -> 9")
        tree.put(3 -> 5, "3-> 5")
        tree.find(2) shouldBe Set("2 -> 2","2 -> 3", "2 -> 5","2 -> 3double")
      }


    }
  }
}

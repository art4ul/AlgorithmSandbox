package com.art4ul.collection.mutable

import org.scalatest.{FunSpec, Matchers}

import scala.util.Random

/**
  * Created by artsemsemianenka on 2/16/16.
  */
class RedBlackTreeSpec extends FunSpec with Matchers {

  describe("RedBlackTree Specification") {

    it("should add sequence of elements and retrieve it back") {
      val randomGenerator = Random
      val tree = new RedBlackTree[Int, Int]
      val elements = (1 to 1000).map(_ => randomGenerator.nextInt())
      elements.foreach(v => tree.put(v, v))
      elements.forall(v => tree.get(v) == Some(v)) shouldBe true
    }

    it("should add sequence of ordered elements and retrieve it back") {
      val tree = new RedBlackTree[Int, Int]
      val elements = (1 to 1000)
      elements.foreach(v => tree.put(v, v))
      elements.forall(v => tree.get(v) == Some(v)) shouldBe true
    }

  }

}

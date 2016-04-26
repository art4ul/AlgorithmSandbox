package com.art4ul.graph.undirected

import com.art4ul.graph.undirected.UndirectedGraph._
import org.scalatest.{FunSpec, Matchers}

/**
  * Created by artsemsemianenka on 4/26/16.
  */
class UndirectedGraphSpec extends FunSpec with Matchers {

  describe("Undirected Graph specification") {
    it("should create empty graph with 4 vertex and fill it") {
      val graph = new UndirectedGraph(4)
      graph += (0 <-> 1) += (0 <-> 2) += (2 <-> 3) += (0 <-> 3)
      graph.adjacent(0).toSet shouldBe Set(1, 2, 3)
      graph.adjacent(1).toSet shouldBe Set(0)
      graph.adjacent(2).toSet shouldBe Set(0, 3)
      graph.adjacent(3).toSet shouldBe Set(0, 2)
    }

    it("should create empty graph with 4 vertex using constructor") {
      val graph = new UndirectedGraph(
        (0 <-> 1),
        (0 <-> 2),
        (2 <-> 3),
        (0 <-> 3)
      )
      graph.adjacent(0).toSet shouldBe Set(1, 2, 3)
      graph.adjacent(1).toSet shouldBe Set(0)
      graph.adjacent(2).toSet shouldBe Set(0, 3)
      graph.adjacent(3).toSet shouldBe Set(0, 2)
    }

    it("should throw exception if number of vertex less then indexes ") {
      intercept[IllegalArgumentException] {
        new UndirectedGraph(
          (0 <-> 2),
          (0 <-> 3),
          (2 <-> 3),
          (0 <-> 3)
        )
      }


    }
  }

}

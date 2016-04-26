package com.art4ul.graph.undirected

import com.art4ul.graph.undirected.UndirectedGraph._
import org.scalatest.{FunSpec, Matchers}

/**
  * Created by artsemsemianenka on 4/26/16.
  */
class GraphSearchSpec extends FunSpec with Matchers {

  describe("Common Search Spec") {

    describe("BreadthFirstSearch"){
      /**
        * Graph :
        *
        *     0 --  2
        *     |  /
        *     1     3
        *
        */
      it("should find connected vertex") {
        val graph = new UndirectedGraph(4)
        graph += 0 <-> 2 += 0 <-> 1 += 1 <-> 2
        val search = new BreadthFirstSearch(graph,0)
        search.connected(2) shouldBe true
        search.connected(1) shouldBe true
        search.connected(3) shouldBe false
        search.vertexConnected() shouldBe 3
      }

      /**
        * Graph :
        *
        *         1 - 3 - 7 - 12
        *       /               \
        *    0                   11
        *      \                /
        *       2 - 4 -5 - 8 - 10
        *            \       /
        *             6 -  9
        */
      it("should return path to vertex"){
        val graph = new UndirectedGraph(
          0 <-> 1,
          0 <-> 2,
          1 <-> 3,
          3 <-> 7,
          7 <-> 12,
          12 <-> 11,
          11 <-> 10,
          10 <-> 8,
          10 <-> 9,
          8 <-> 5,
          5 <-> 4,
          9 <-> 6,
          6 <-> 4,
          4 <-> 2
        )

        val search = new BreadthFirstSearch(graph,0)
        search.hasPathTo(7) shouldBe true
        search.path(7) shouldBe List(0,1,3,7)

        search.hasPathTo(8) shouldBe true
        search.path(8) shouldBe List(0,2,4,5,8)
      }
    }


    describe("DepthFirstSearch"){

      /**
        * Graph :
        *
        *     0 ->  2
        *     |  /
        *     V/
        *     1     3
        */
      it("should find connected vertex") {
        val graph = new UndirectedGraph(4)
        graph += 0 <-> 2 += 0 <-> 1 += 1 <-> 2
        val search = new DepthFirstSearch(graph,0)
        search.connected(2) shouldBe true
        search.connected(1) shouldBe true
        search.connected(3) shouldBe false
        search.vertexConnected() shouldBe 3
      }

      /**
        * Graph :
        *
        *         1 - 3 - 8
        *       /
        *    0        6
        *      \     / \
        *       2 - 4   7
        *            \ /
        *             5
        */
      it("should return path to vertex"){
        val graph = new UndirectedGraph(
          0 <-> 1,
          0 <-> 2,
          1 <-> 3,
          3 <-> 8,
          2 <-> 4,
          4 <-> 6,
          4 <-> 5,
          6 <-> 7,
          5 <-> 7
        )

        val search = new DepthFirstSearch(graph,0)
        search.hasPathTo(8) shouldBe true
        search.path(8) shouldBe List(0,1,3,8)

        search.hasPathTo(7) shouldBe true
        search.path(6) shouldBe List(0,2,4,5,7,6)
      }
    }

  }

}

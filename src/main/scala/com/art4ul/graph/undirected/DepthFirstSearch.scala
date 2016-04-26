package com.art4ul.graph.undirected

import com.art4ul.graph.Graph

import scala.collection.mutable

/**
  * Created by artsemsemianenka on 4/26/16.
  */
class DepthFirstSearch private (override val graph: Graph) extends CommonSearch {

  def this(graph: Graph, startVertex: Int)={
    this(graph)
    init(startVertex)
  }

  val storage = mutable.Stack[Int]()

  override protected def loadVertex(): Int = storage.pop()

  override protected def storeVertex(vertex: Int): Unit = storage.push(vertex)

  override protected def isStorageEmpty: Boolean = storage.isEmpty


}

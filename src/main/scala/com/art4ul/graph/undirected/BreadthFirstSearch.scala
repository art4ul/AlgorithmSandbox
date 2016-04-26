package com.art4ul.graph.undirected

import com.art4ul.graph.Graph

import scala.collection.mutable

/**
  * Created by artsemsemianenka on 4/26/16.
  */
class BreadthFirstSearch private(override val graph: Graph) extends CommonSearch {

  def this(graph: Graph, startVertex: Int)={
    this(graph)
    init(startVertex)
  }

  val storage = mutable.Queue[Int]()

  override protected def loadVertex(): Int = storage.dequeue()

  override protected def storeVertex(vertex: Int): Unit = storage.enqueue(vertex)

  override protected def isStorageEmpty: Boolean = storage.isEmpty


}

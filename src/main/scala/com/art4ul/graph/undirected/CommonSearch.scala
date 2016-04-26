package com.art4ul.graph.undirected

import com.art4ul.graph.{Path, Graph, Search}

import scala.annotation.tailrec

/**
  * Created by artsemsemianenka on 4/26/16.
  */
trait CommonSearch extends Search with Path{
  val graph: Graph

  protected def storeVertex(vertex: Int): Unit

  protected def loadVertex(): Int

  protected def isStorageEmpty: Boolean

  val marked: Array[Boolean] = Array.fill(graph.vertexCount)(false)
  val edgeTo: Array[Option[Int]] = Array.fill(graph.vertexCount)(None)

  protected def init(startVertex: Int): Unit = {
    storeVertex(startVertex)
    while (!isStorageEmpty) {
      val vertex = loadVertex
      marked(vertex) = true
      val adjs = graph.adjacent(vertex)
      adjs.view
        .filter(v => marked(v) == false)
        .foreach { v =>
          edgeTo(v) = Some(vertex)
          storeVertex(v)
        }
    }
  }

  override def connected(v: Int): Boolean = marked(v)

  override def vertexConnected(): Int = marked.filter(_ == true).length

  override def hasPathTo(v: Int): Boolean = marked(v)

  override def path(vertex: Int): List[Int] = {
    @tailrec
    def inner(elem:Option[Int],res:List[Int]):List[Int] =elem match {
      case None => res
      case Some(v) => inner(edgeTo(v),v :: res)
    }
    inner(edgeTo(vertex),List(vertex))
  }
}

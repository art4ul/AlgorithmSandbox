package com.art4ul.graph

/**
  * Created by artsemsemianenka on 4/26/16.
  */
case class UndirectedGraph(vertex: Array[List[Int]]) extends Graph{

  def this(vertexNumber: Int) = this(Array.fill(vertexNumber)(List[Int]()))

  def this(edges: (Int, Int)*) = {
    this {
      val result = edges.flatMap { case (v, w) => Seq(v -> w, w -> v) }
        .groupBy { case (key, _) => key }
        .mapValues(seq => seq.toList.map { case (_, value) => value })
        .toSeq
        .sortBy(_._1)

      val checked = result.zipWithIndex.forall { case ((key, _), index) => key == index }
      require(checked, "All vertex should present. Indexing starts form 0 position")

      result.map { case (_, value) => value }.toArray
    }
  }

  def +=(edge:(Int,Int)): this.type = {
    val (first,second) = edge
    val firstVertexList = vertex(first)
    val secondVertexList = vertex(second)
    vertex(first) = firstVertexList :+ second
    vertex(second) = secondVertexList :+ first
    this
  }

  def adjacent(indx:Int):Seq[Int]= vertex(indx)

}

object UndirectedGraph{

  implicit class DoubleArrow[A](private val self: A) extends AnyVal {
    @inline def <->[B](y: B): Tuple2[A, B] = self -> y
  }
}

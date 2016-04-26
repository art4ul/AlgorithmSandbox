package com.art4ul.graph

/**
  * Created by artsemsemianenka on 4/26/16.
  */
trait Graph {

  def +=(edge:(Int,Int)): this.type

  def adjacent(indx:Int):Seq[Int]

}

package com.art4ul.graph

/**
  * Created by artsemsemianenka on 4/26/16.
  */
trait Path {

  def hasPathTo(v:Int):Boolean

  def path(v:Int):Seq[Int]

}

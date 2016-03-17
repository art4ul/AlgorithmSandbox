package com.art4ul.collection.mutable

/**
  * Created by artsemsemianenka on 2/23/16.
  */


case class Range[T <% Ordered[T]](low: T, hi: T) extends Ordered[Range[T]]{

  override def compare(that: Range[T]): Int = {
    val res = this.low.compare(that.low)
    if (res == 0) this.hi.compare(that.hi) else res
  }

  def intersect(point:T):Boolean = (hi >= point) && (low <= point)

}


class IntervalTree[K <% Ordered[K], V] {


  sealed case class Node(key: Range[K],
                         var value: Set[V],
                         var max: K,
                         var left: Node = null,
                         var right: Node = null,
                         var red: Boolean = false) extends Serializable

  private var root: Option[Node] = None

  def put(key: (K, K), value: V): Unit = put(Range(key._1, key._2), value)

  def put(key: Range[K], value: V): Unit = root match {
    case None => root = Some(Node(key, Set(value), key.hi))
    case Some(node) => root = Some(put(key, value, node))
  }

  private def max(first: K, second: K): K =
    if (first.compare(second) >= 0) first else second


  private def calcMaxKey(node: Node): Unit = {
    val maxKey = node match {
      case n if n.left == null && n.right == null => n.key.hi
      case n if n.left == null => max(n.right.max, n.key.hi)
      case n if n.right == null => max(n.left.max, n.key.hi)
      case _ => max(node.left.max, node.right.max)
    }
    node.max = maxKey
  }


  private def put(key: Range[K], value: V, parent: Node): Node = {
    if (parent == null) {
      Node(key, Set(value), key.hi, red = true)
    } else {
      var resultNode = parent match {
        case node if (node.key == key) =>
          node.value = node.value ++ Set(value)
          node

        case node if (node != null && key > node.key) =>
          node.right = put(key, value, node.right)
          node

        case node if (node != null && key < node.key) =>
          node.left = put(key, value, node.left)
          node
      }

      if (isRed(resultNode.right) && !isRed(resultNode.left)) resultNode = rotateLeft(resultNode)
      if (isRed(resultNode.left) && isRed(resultNode.left.left)) resultNode = rotateRight(resultNode)
      if (isRed(resultNode.left) && isRed(resultNode.right)) flipColor(resultNode)
      calcMaxKey(resultNode)
      resultNode
    }

  }

  protected[mutable] def rotateRight(initNode: Node): Node = {
    val resNode = initNode.left
    initNode.left = resNode.right
    resNode.right = initNode
    resNode.red = resNode.right.red
    resNode.right.red = true
    calcMaxKey(initNode)
    resNode
  }

  protected[mutable] def rotateLeft(initNode: Node): Node = {
    val resNode = initNode.right
    initNode.right = resNode.left
    resNode.left = initNode
    resNode.red = resNode.left.red
    resNode.left.red = true
    calcMaxKey(initNode)
    resNode
  }

  protected[mutable] def isRed(node: Node): Boolean = if (node != null) node.red else false

  protected[mutable] def invertColor(node: Node): Unit = if (node != null) node.red = !node.red

  protected[mutable] def flipColor(node: Node): Unit = {
    invertColor(node)
    invertColor(node.left)
    invertColor(node.right)
  }

  def isEmpty: Boolean = root.isDefined



  protected[mutable] def find(point: K, parent: Node): Set[V] = {
    val intersectedValue:Set[V] = if (parent.key.intersect(point)) parent.value else Set()
    parent match {

      case node if (point >= node.key.low  && point<= node.max) =>
        val rightSet = if (node.right != null) find(point, node.right) else Set()
        val leftSet = if (node.left != null) find(point, node.left) else Set()
        intersectedValue ++ rightSet ++ leftSet

      case node if (point < node.key.low &&  node.left != null && point<= node.max) =>
        intersectedValue ++ find(point, node.left)

      case _ => intersectedValue
    }
  }

 def find(point: K): Set[V] = root.map(find(point, _)).getOrElse(Set())
}

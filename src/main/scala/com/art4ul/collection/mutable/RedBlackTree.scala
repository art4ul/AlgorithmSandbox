package com.art4ul.collection.mutable

/**
  * Created by artsemsemianenka on 2/16/16.
  */


class RedBlackTree[K<%Ordered[K],V] extends Serializable{

  sealed case class Node(key: K,
                  var value: V,
                  var left: Node = null,
                  var right: Node = null,
                  var red: Boolean = false) extends Serializable

  private var root: Option[Node] = None

  def put(key: K, value: V): Unit = root match {
    case None => root = Some(Node(key, value))
    case Some(node) => root = Some(put(key, value, node))
  }

  private def put(key: K, value: V, parent: Node): Node = {
    if (parent == null) {
      Node(key, value, red = true)
    } else {
      var resultNode = parent match {
        case node if (node.key == key) =>
          node.value = value
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
      resultNode
    }

  }

  protected[mutable] def rotateRight(initNode: Node): Node = {
    val resNode = initNode.left
    initNode.left = resNode.right
    resNode.right = initNode
    resNode.red = resNode.right.red
    resNode.right.red = true
    resNode
  }

  protected[mutable] def rotateLeft(initNode: Node): Node = {
    val resNode = initNode.right
    initNode.right = resNode.left
    resNode.left = initNode
    resNode.red = resNode.left.red
    resNode.left.red = true
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

  protected[mutable]  def get(key: K, parent: Node): Option[V] = parent match {
    case node if (node.key == key) => Some(node.value)

    case node if (key > node.key && node.right != null) => get(key, node.right)

    case node if (key < node.key && node.left != null) => get(key, node.left)

    case _ => None
  }

  def get(key: K): Option[V] = root.flatMap(get(key, _))

}


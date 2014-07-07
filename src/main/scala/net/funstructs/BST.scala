package net.funstructs

/**
 * Created by jeff on 7/5/14.
 */
sealed abstract class BST[+A] {
}

case class Node[+A](value: A, left: BST[A], right: BST[A]) extends BST[A] {
  override def toString = s"($left, $value, $right)"
}
case object E extends BST[Nothing] {
  override def toString = "(empty)"
}

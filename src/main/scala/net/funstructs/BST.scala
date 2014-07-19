package net.funstructs

/**
 * Created by jeff on 7/5/14.
 */
sealed abstract class BST[+A <% Ordered[A]] {
  def member[X >: A <% Ordered[X]](e: X): Boolean
  def insert[X >: A <% Ordered[X]](e: X): BST[X]
  def map[B](f: A => B): List[B]
}

// Empty nodes
case object E extends BST[Nothing] {

  override def toString = "(empty)"

  override def member[X <% Ordered[X]](e: X): Boolean = false

  override def insert[X <% Ordered[X]](e: X): BST[X] = Node[X](e, left = E, right = E)

  override def map[B](f: (Nothing) => B): List[B] = List[B]()
}


// Populated...
case class Node[+A <% Ordered[A]](value: A, left: BST[A], right: BST[A]) extends BST[A] {

  override def toString = s"($left, $value, $right)"

  override def member[X >: A <% Ordered[X]](e: X): Boolean = e match {
    case x if x < value => left.member(x)
    case x if x > value => right.member(x)
    case _ => true // ==
  }

  override def insert[X >: A <% Ordered[X]](e: X): BST[X] = e match {
    case x if x < value => Node[X](value, left = left.insert(e), right)
    case x if x > value => Node[X](value, left, right = right.insert(e))
    case _ => this // ==
  }

  // TODO not the best impl of map, slow and inefficient
  override def map[B](f: A => B): List[B] = f(value) :: left.map(f) ::: right.map(f)
}

object BST {
  def empty[B <% Ordered[B]]: BST[B] = E
  def apply[B <% Ordered[B]](xs: B*) = 
    (xs foldLeft BST.empty[B]) { (acc: BST[B], e: B) => acc.insert(e) }
}

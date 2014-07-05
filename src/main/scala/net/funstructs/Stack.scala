package net.funstructs

/**
 * Created by jeff on 7/4/14.
 */
trait Stack {
  type S // type of stack
  type A // type of elems

  def empty: S
  def isEmpty(s: S): Boolean
  def cons(a: A, s: S): S
  def head(s: S): A
  def tail(s: S): S
  def suffix(s: S): List[List[A]]
}

trait ListStack extends Stack {
  override type S = List[A]

  override def empty: S = Nil
  override def isEmpty(s: S): Boolean = s.isEmpty

  override def cons(a: A, s: S): S = a :: s
  override def head(s: S): A = s.head
  override def tail(s: S): S = s.tail
  override def suffix(s: S): List[List[A]] = {
    if (isEmpty(s)) List(List())
    else s :: suffix(tail(s))
  }
}

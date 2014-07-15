package net.funstructs.heap

/**
 * Created by jeff on 7/12/14.
 */
sealed abstract class Heap[+A <% Ordered[A]] {
  def insert[B >: A <% Ordered[B]](x: B): Heap[B]
  def merge[B >: A <% Ordered[B]](other: Heap[B]): Heap[B]

  def findMin: Integer
  def deleteMin: Heap[A]
}

case object E extends Heap[Nothing] {
  override def deleteMin: Heap[Nothing] = ???
  override def findMin: Integer = ???
  override def insert[B <% Ordered[B]](x: B): Heap[B] = Branch(min=x, left=E, right=E, rank=1)
  override def merge[B <% Ordered[B]](other: Heap[B]): Heap[B] = ???
}

case class Branch[A <% Ordered[A]](min: A,
                                   left: Heap[A],
                                   right: Heap[A],
                                   rank: Int) extends Heap[A] {
  override def insert[B >: A <% Ordered[B]](x: B): Heap[B] = ???
  override def deleteMin: Heap[A] = ???
  override def findMin: Integer = ???
  override def merge[B >: A <% Ordered[B]](other: Heap[B]): Heap[B] = ???
}

object Heap {
  def empty[B <% Ordered[B]]: Heap[B] = E
  def apply[B <% Ordered[B]](elems: B*): Heap[B] =
    (elems foldLeft Heap.empty[B]) { (acc: Heap[B], e: B) => acc.insert(e) }
}

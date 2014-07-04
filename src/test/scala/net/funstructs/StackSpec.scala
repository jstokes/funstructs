package net.funstructs

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

/**
 * Created by jeff on 7/4/14.
 */
object StackSpec extends Properties("Stack") with ListStack {

  override type A = AnyVal

  lazy val genStack: Gen[S] = for {
    n <- arbitrary[AnyVal]
    s <- oneOf(const(empty), genStack)
  } yield cons(n, s)

  property("head of a single item stack is that item") = forAll { n: A =>
    val stack = cons(n, empty)
    head(stack) == n
  }

  property("tail of a single item stack is empty") = forAll { n: A =>
    val stack = cons(n, empty)
    isEmpty(tail(stack))
  }

  property("stacks are LIFO (last in first out)") = forAll { aList: List[A] =>
    (aList.size > 0) ==> {
      val stack = aList.foldRight(empty) { (v: A, acc: S) => cons(v, acc) }
      head(stack) == aList.head
    }
  }

  property("head of an empty stack throws an exception") = {
    throws(classOf[NoSuchElementException]) { head(empty) }
  }
}

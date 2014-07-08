package net.funstructs

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

/**
 * Created by jeff on 7/5/14.
 */
object BSTSpec extends Properties("Stack") {

  type IntTree = BST[Int]

  lazy val genBST: Gen[IntTree] = for {
    n <- arbitrary[Int]
    s <- oneOf(const(E), genBST)
  } yield s.insert(n)

  implicit def arbBST = Arbitrary(genBST)

  property("have smaller keys on the left, larger on the right") = forAll { tree: IntTree =>
    def check(tree: IntTree): Boolean = tree match {
      case E => true
      case Node(_, E, E) => true
      case Node(x, E, r: Node[Int]) if x < r.value => check(r)
      case Node(x, l: Node[Int], E) if x > l.value => check(l)
      case Node(x, l: Node[Int], r: Node[Int]) if x > l.value && x < r.value => check(l) && check(r)
      case _ => false
    }
    check(tree)
  }
  property("contain no duplicate entries") = forAll { tree: IntTree =>
    val values = tree map identity
    values.toSet.size == values.size
  }
}
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
  type IntNode = Node[Int]

  lazy val genBST: Gen[IntTree] = for {
    n <- arbitrary[Int]
    s <- oneOf(const(E), genBST)
  } yield s.insert(n)

  implicit def arbBST = Arbitrary(genBST)

  property("have smaller keys on the left, larger on the right") = forAll { tree: IntTree =>
    def check(tree: IntTree): Boolean = tree match {
      case E => true
      case Node(_, E, E) => true
      case Node(x, E, r: IntNode) if x < r.value => check(r)
      case Node(x, l: IntNode, E) if x > l.value => check(l)
      case Node(x, l: IntNode, r: IntNode) if x > l.value && x < r.value => check(l) && check(r)
      case _ => false
    }
    check(tree)
  }
  property("contain no duplicate entries") = forAll { tree: IntTree =>
    val values = tree map identity
    values.toSet.size ?= values.size
  }

  property("keeps track of its members") = forAll { tree: IntTree =>
    tree map identity forall (v => tree.member(v))
  }

  property("member of an empty bst is always false") = forAll { a: Int =>
    !E.member(a)
  }

  property("provides a sane toString") = {
    val tree = Node(4, Node(2, E, E), Node(7, E, E))
    tree.toString ?= "(((empty), 2, (empty)), 4, ((empty), 7, (empty)))"
  }

  property("can be constructed by values") = {
    BST(1, 2, 3) ?= BST(1, 2, 3)
  }
}

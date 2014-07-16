package net.funstructs

import net.funstructs.heap.Heap
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

/**
 * Created by jeff on 7/13/14.
 */
object HeapSpec extends Properties("Stack") {

  property("can construct themselves from elements") = {
    Heap(1) != null
  }

  property("min of a single element heap is that element") = {
    Heap(1).findMin == 1
  }
}
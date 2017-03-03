package cofree
package test

import org.scalatest._
import org.scalatest.prop._
import org.scalacheck._
import Arbitrary._
import org.scalacheck.Prop._

class BTreeSpec extends FlatSpec with Matchers with Checkers {
  import BTree._

  def balanced[A](t: BTree[A]): Boolean = 

  "this check" should "fail" in check {(is: Nel[Int]) =>
    is.tail.foldLeft(leaf(is.head))(_ insert _)
	true
  }
}

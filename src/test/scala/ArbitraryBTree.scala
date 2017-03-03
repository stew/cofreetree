package cofree
package test

import cats._
import dogs._
import dogs.tests.arbitrary.list._
import dogs.Predef._

import org.scalacheck._

trait ArbitraryBTree {
  import BTree._
  import BTreeLoc._

  implicit def arbitraryBTree[A: Arbitrary: Order]: Arbitrary[BTree[A]] =
    Arbitrary(nelGen[A].map(nel => nel.tail.foldLeft(leaf(nel.head))(_ insert _)))
}

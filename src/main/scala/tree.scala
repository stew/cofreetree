package cofree

import cats._
import cats.implicits._
import cats.syntax.foldable._
import dogs._
import dogs.syntax.birds._
import dogs.Predef._
import Option._
import scala.inline
import scala.{Either,Left,Right}

import BTreeLoc._
import Cofree._

object BTree {
  type Children[A] = (Option[A], Option[A])

  // We can define a non-empty binary tree as being a Cofree using the
  // above pattern, with each node labelled by both a value, and
  // memoized height of the tree.
  type BTree[A] = Cofree[Children, (A,Int)]

  @inline def extract[A](x: BTree[A]) = x.head._1
  @inline def height[A](x: BTree[A]): Int = x.head._2


  implicit val childrenFoldable: Foldable[Children] = new Foldable[Children] {
    def foldLeft[A, B](fa: Children[A], b: B)(f: (B, A) => B): B  =
      fa._2.foldLeft(fa._1.foldLeft(b)(f))(f)
    def foldRight[A, B](fa: Children[A], b: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]  =
      fa._1.foldRight(fa._2.foldRight(b)(f))(f)
    }

//  implicit val btreeReducible: Reducible[BTree] = Cofree.cofreeReducible[Children]
//  implicit val locReducible: Reducible[BTreeLoc] = Cofree.cofreeReducible

  // A constructor for creating a node of the tree, given a value, and
  // two possible child trees
  def node[A](left: Option[BTree[A]], a: A, right: Option[BTree[A]]): BTree[A] = {
    val h = java.lang.Math.max(left.cata(height, 0), right.cata(height,0)) + 1
    Cofree[Children, (A,Int)]((a,h), (left, right))
  }

  // A constructor for creating a single element tree
  def leaf[A](a: A): BTree[A] = Cofree[Children,(A,Int)]((a,1), (None, None))

  @inline def left[A](t: BTree[A]): Option[BTree[A]] = t.tail._1
  @inline def right[A](t: BTree[A]): Option[BTree[A]] = t.tail._2

    // we are grafting the tree `a` to the location `loc`, we want to
    // build a new balanced tree by balancing as we work our way upward.
    // we assume that t is already balanced
  private def graft[A](t: BTree[A], loc: BTreeLoc[A]): BTree[A] = {


    implicit val locfold: Foldable[BTreeLoc] = Cofree.cofreeReducible[BTreeLoc](BTreeLoc.foldableParents)

    val newTree = loc.foldLeft(t){(t, oldTree) =>
      oldTree match {
        case Some(Left(Cofree(head,_))) ⇒
          node(left(head), extract(head), some(t))

        case Some(Right(Cofree(head,_))) ⇒
          node(some(t), extract(head), right(head))

        case None ⇒ t
      }
    }

    balance(newTree)
  }

    // this finds the location in the tree closest to where the given
    // element would live.
  private def findNode[A](t: BTree[A], a: A)(implicit order: Order[A]): (Int, BTreeLoc[A]) = {
      @tailrec def loop(tl: BTreeLoc[A]): (Int, BTreeLoc[A]) =
        order.compare(a, extract(tl.head)) match {
          case 0 ⇒
            (0, tl)
          case x if x < 0 ⇒
            BTreeLoc.left(tl) match {
              case Some(tl) ⇒ loop(tl)
              case _ ⇒ (-1, tl)
            }
          case _ ⇒
            BTreeLoc.right(tl) match {
              case Some(tl) ⇒
                loop(tl)
              case _ ⇒
                (1, tl)
            }
        }
      loop(rootLoc(t))
    }

    def delete[A](a: A)(t: BTree[A])(implicit order: Order[A]): Option[BTree[A]] = {
      def removeLeast(bt: BTree[A]): (A, Option[BTree[A]]) = {
        val bottom = least(rootLoc(bt))
        val a = extract(bottom.head)
        BTreeLoc.right(bottom) match {
          case None ⇒ up(bottom) match {
            case None ⇒ (a, None)
            case Some(parent) ⇒
              val newTree = node(None, extract(parent.head), right(parent.head))
              (a, Some(graft(newTree, parent)))
          }
          case Some(t) ⇒
            up(bottom) match {
              case None ⇒ (a, Some(t.head))
              case Some(parent) ⇒ (a, Some(graft(t.head, parent)))
            }
        }
      }

      val (o, loc) = findNode(t, a)
      if(o == 0) (left(loc.head), right(loc.head)) match {
        case (Some(l), Some(r)) ⇒
          val (removed,remains) = removeLeast(r)
          val newTree = node(Some(l), removed, remains)
          Some(graft(newTree, loc))
        case (None, Some(x)) ⇒ Some(graft(x, loc))
        case (Some(y), None) ⇒ Some(graft(y, loc))
        case (None,None) ⇒ None
      }
      else Some(t)
    }


  def insert[A](a: A)(t: BTree[A])(implicit order: Order[A]): BTree[A] = {
      val (o, loc) = findNode(t, a)

      if(o == 0) t
      else if (o < 0) {
        val n = node(some(leaf(a)), extract(loc.head), right(loc.head))
        graft(n, loc)


      } else {
        val n = node(left(loc.head), extract(loc.head), some(leaf(a)))
        graft(n, loc)
      }
    }

  private def balance[A](t: BTree[A]): BTree[A] = {
      def rotation(l: Int, r: Int, allow: Int): Int =
        if(l - r > allow ) 1
        else if(r - l > allow) -1
        else 0

      rotation(right(t).cata(height,0), left(t).cata(height,0), 1) match {
        case 0 ⇒ t
        case x if x > 0 ⇒
          right(t) match {
            case None ⇒ t
            case Some(Cofree((rv,_), (rl, rr))) ⇒
              if(rotation(rl.cata(height,0), rr.cata(height,0), 0) > 0 ) {
                val Some(Cofree((rlv,_), (rll, rlr))) = rl
                node(Some(node(left(t), extract(t), rll)), rlv, Some(node(rlr, rv, rr)))
              } else {
                node(Some(node(left(t), extract(t), rl)), rv, rr)
              }
          }
        case _ ⇒
          left(t) match {
            case None ⇒ t
            case Some(Cofree((lv,_), (ll, lr))) ⇒
              if(rotation(ll.cata(height,0), lr.cata(height,0), 0) < 0) {
                val Some(Cofree((lrv,_), (lrl, lrr))) = lr
                node[A](Some(node(ll, lv, lrl)), lrv, Some(node(lrr, extract(t), right(t))))
              } else {
                node[A](ll, lv, Some(node(lr, extract(t), right(t))))
              }
          }
      }
    }

  implicit class BTreeOps[A](t: BTree[A]) {
    def show: String =
      "[" + BTree.left(t).cata(_.show, "") + "(" + extract(t) + ")" + BTree.right(t).cata(_.show, "") + "]"

    def left: Option[BTree[A]] = BTree.left(t)
    def right: Option[BTree[A]] = BTree.right(t)

    def delete(a: A)(implicit order: Order[A]): Option[BTree[A]] = BTree.delete(a)(t)
    def insert(a: A)(implicit order: Order[A]): BTree[A] = BTree.insert(a)(t)

  }
}

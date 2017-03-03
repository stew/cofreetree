package cofree

import dogs.Predef._
import dogs._
import cats._
import cats.implicits._
import scala.{Either,Left,Right}

object BTreeLoc {
  import Option._
  import BTree._

  // Option[Codiagonal[A]]?
  type Parents[A] = Option[Either[A, A]]
  type BTreeLoc[A] = Cofree[Parents, BTree[A]] 

  def rootLoc[A](bt: BTree[A]): BTreeLoc[A] = Cofree[Parents, BTree[A]](bt, none)


  implicit val foldableParents: Foldable[Parents] = new Foldable[Parents] {
    def foldLeft[A, B](fa: Parents[A], b: B)(f: (B, A) => B): B  =
      fa match {
        case None => b
        case Some(Left(a)) => f(b,a)
        case Some(Right(a)) => f(b,a)
      }

    def foldRight[A, B](fa: Parents[A], b: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B]  =
      fa match {
        case None => b
        case Some(Left(a)) => f(a,b)
        case Some(Right(a)) => f(a,b)
      }
  }

  @tailrec def least[A](bt: BTreeLoc[A]): BTreeLoc[A] = left(bt) match {
    case Some(bt) ⇒ least(bt)
    case None ⇒ bt
  }

  def pathStr[A](loc: BTreeLoc[A]): String = {
    def loop(loc: BTreeLoc[A], str: String): String =
      loc.tail match {
        case None ⇒ str
        case Some(Left(t)) ⇒ loop(t, "r" + str)
        case Some(Right(t)) ⇒ loop(t, "l" + str)
      }
    loop(loc, "")
  }

  implicit def showLoc[A:Show](loc: BTreeLoc[A]): String =
    s"<Loc: path: ${pathStr(loc)} in ${root(loc).show}>"

  @tailrec
  private def leftParent[A](loc: BTreeLoc[A]): Option[BTreeLoc[A]] =
    loc.tail match {
      case Some(Left(x)) ⇒ some(x)
      case Some(Right(x)) ⇒ leftParent(x)
      case _ ⇒ none
    }

  @tailrec
  private def rightParent[A](loc: BTreeLoc[A]): Option[BTreeLoc[A]] =
    loc.tail match {
      case Some(Left(x)) ⇒ rightParent(x)
      case Some(Right(x)) ⇒ some(x)
      case _ ⇒ none
    }

  def up[A](loc: BTreeLoc[A]): Option[BTreeLoc[A]] =
    loc.tail match {
      case Some(Left(x)) ⇒ some(x)
      case Some(Right(x)) ⇒ some(x)
      case _ ⇒ none
    }

  @tailrec
  def root[A](loc: BTreeLoc[A]): BTree[A] = loc.tail match {
    case Some(Left(x)) ⇒ root(x)
    case Some(Right(x)) ⇒ root(x)
    case _ ⇒ loc.head
  }

  def prev[A](loc: BTreeLoc[A])(implicit order: Order[A]): Option[BTreeLoc[A]] =
    left(loc) orElse leftParent(loc)

  def next[A](loc: BTreeLoc[A])(implicit order: Order[A]): Option[BTreeLoc[A]] =
    right(loc) orElse rightParent(loc)

  def left[A](loc: BTreeLoc[A]): Option[BTreeLoc[A]] = BTree.left(loc.head).map { child ⇒
    Cofree[Parents, BTree[A]](child, some(Right(loc)))
  }

  def right[A](loc: BTreeLoc[A]): Option[BTreeLoc[A]] = BTree.right(loc.head).map { child ⇒
    Cofree[Parents, BTree[A]](child, some(Left(loc)))
  }


  def extract[A](loc: BTreeLoc[A]): A = loc.head.head._1
}




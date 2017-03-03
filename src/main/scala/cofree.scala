package cofree

import cats._

case class Cofree[F[_], A](head: A, tail: F[Cofree[F, A]]) 

object Cofree {

  implicit def cofreeReducible[F[_]](implicit F: Foldable[F]): Reducible[Cofree[F,?]] =
    new Reducible[Cofree[F,?]] {

      override def reduceLeftTo[A, B](fa: Cofree[F,A])(f: A => B)(g: (B, A) => B): B =
        F.foldLeft(fa.tail, f(fa.head))((b,cfa) ⇒ g(b,cfa.head))

      override def reduceRightTo[A, B](fa: Cofree[F,A])(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] =
        F.foldRight(fa.tail, Eval.always(f(fa.head)))((cfa,b) ⇒ g(cfa.head,b))

      override def foldLeft[A, B](fa: cofree.Cofree[F,A],b: B)(f: (B, A) => B): B =
        F.foldLeft(fa.tail, f(b,fa.head))((b,cfa) ⇒ f(b, cfa.head))

      override def foldRight[A, B](fa: cofree.Cofree[F,A],lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] =
        F.foldRight(fa.tail, f(fa.head,lb))((cfa, b) ⇒ f(cfa.head, b))

    }
}

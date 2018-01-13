package learnfp.free

import learnfp.free
import learnfp.monad.{Monad, MonadOps}
import learnfp.functor.{Functor, FunctorOps}
import learnfp.monad.MonadOps._

import scala.annotation.tailrec

sealed trait Free[F[_], A]
final case class Return[F[_], A](a:A) extends Free[F, A]
final case class FlatMap[F[_], A, B](a:Free[F, A], fx:A => Free[F, B]) extends Free[F, B]
final case class LiftF[F[_], A](fn:F[A]) extends Free[F, A]

abstract class Natural[F[_], G[_]] {
  def transform[A](a:F[A]):G[A]
}

object Free {
  implicit def freeFunctorInstance[F[_]] = new Functor[({type E[X] = Free[F, X]})#E] {
    override def fmap[A, B](a: Free[F, A])(fx: A => B): Free[F, B] = {
      FlatMap(a, (x: A) => Return(fx(x)))
    }
  }

  implicit def freeToFunctorOps[F[_], A](a:Free[F, A]) = new FunctorOps[A, ({type E[X] = Free[F, X]})#E](a)

  implicit def freeMonadInstance[F[_]] = new Monad[({type E[X] = Free[F, X]})#E] {
    override def pure[A](a: A): Free[F, A] = Return(a)
    override def flatMap[A, B](a: Free[F, A])(fx: A => Free[F, B]): Free[F, B] = {
      FlatMap(a, fx)
    }
  }

  implicit def freeToMonadOps[F[_], A](a:Free[F, A]) = new MonadOps[A, ({type E[X] = Free[F, X]})#E](a)

  def liftF[F[_], A](a:F[A]):Free[F, A] = LiftF[F, A](a)

  def foldF[F[_], M[_], A](af:Free[F, A])(trans:Natural[F, M])(implicit f:Functor[M], m:Monad[M]):M[A] = {
    af match {
      case Return(x) => m.pure(x)
      case LiftF(fn) => trans.transform(fn)
      case FlatMap(Return(x), fx) => foldF(fx(x))(trans)
      case FlatMap(LiftF(fn: F[A]), fx) => // fn: F[A], fx: A => Free[F, B]
        val ma: M[A] = trans.transform(fn)
        m.flatMap(ma) { a: A =>
          foldF(fx(a))(trans) // fx(a): Free[F, B]
        }
      case FlatMap(FlatMap(f0: Free[F, A], fb), fg) => // f0: F[A''], fb: A'' => Free[F, A'], fg: A' => Free[F, A]
        foldF(FlatMap(f0, (a: A) => FlatMap(fb(a), fg)))(trans)
    }
  }
}
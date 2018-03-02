package learnfp.transformer

import learnfp.functor.{Functor, FunctorOps, Id}
import learnfp.monad.{Monad, MonadOps, MonadOpsPure}
import learnfp.functor.IdInstance._
import learnfp.functor.FunctorOps._
import learnfp.monad.MonadOps._

case class IdT[A, F[_]](runIdT:F[Id[A]])

object IdT {
  implicit def idtFunctorInstance[F[_]](implicit outerFunctor:Functor[F]) = new Functor[({type E[X] = IdT[X, F]})#E] {
    override def fmap[A, B](a: IdT[A, F])(fx: A => B): IdT[B, F] = {
      IdT(a.runIdT.fmap(_.fmap(fx)))
    }
  }

  implicit def idtMonadInstance[M[_]](implicit outerMonad:Monad[M], outerFunctor:Functor[M]) = new Monad[({type E[X] = IdT[X, M]})#E] {
    override def pure[A](a: A): IdT[A, M] = IdT(Id(a).pure[M])
    override def flatMap[A, B](a: IdT[A, M])(fx: A => IdT[B, M]): IdT[B, M] = {
      val bm = a.runIdT.flatMap { fa =>
        fa.map(fx).value.runIdT
      }
      IdT(bm)
    }
  }

  implicit def idtToMonadOps[A, M[_]](a:IdT[A, M])(implicit m:Monad[M], f:Functor[M]) =
    new MonadOps[A, ({type E[X] = IdT[X, M]})#E](a)

  implicit def idtMonadTransInstance[M[_]](implicit m:Monad[M], f:Functor[M]) = new MonadTransformer[M, IdT] {
    override def lift[A](a: M[A]): IdT[A, M] = {
      IdT(a.map(Id(_)))
    }
  }

  def lift[A, M[_]](a:M[A])(implicit f:Functor[M], m:Monad[M]):IdT[A, M] = idtMonadTransInstance[M].lift(a)
}

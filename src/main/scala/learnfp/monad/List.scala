package learnfp.monad

import learnfp.functor.ListInstance._

import scala.annotation.tailrec

object ListInstance {
  implicit val listMonadInstance = new Monad[List] {
    override def pure[A](a: A): List[A] = List(a)
    override def flatMap[A, B](a: List[A])(fx: A => List[B]): List[B] = {
      a.flatMap(fx)
    }
  }
}

package learnfp.monad

import learnfp.functor.State
import learnfp.functor.StateInstance._

object StateInstance {
  implicit def stateMonadInstance[S] = new Monad[({type E[X] = State[S, X]})#E]() {
    override def pure[A](a: A): State[S, A] = State(s => (s, a))
    override def flatMap[A, B](a: State[S, A])(fx: A => State[S, B]): State[S, B] = {
      State(
        s => {
          val (sa, ra) = a.run(s)
          fx(ra).run(sa)
        }
      )
    }
  }

  implicit def stateToMonadOps[S, A](a:State[S, A]) = new MonadOps[A, ({type E[X] = State[S, X]})#E](a)
}

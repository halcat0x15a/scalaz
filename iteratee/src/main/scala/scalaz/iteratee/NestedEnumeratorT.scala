package scalaz
package iteratee

import Iteratee._

private[iteratee] class NestedEnumeratorT[X, E] {
  // frequently used type lambdas
  def lift[A, F[_]: Monad](iter: IterateeT[X, E, F, A]): IterateeT[X, E, ({type λ[α] = IterateeT[X, E, F, α]})#λ, A] = IterateeT.IterateeTMonadTrans[X, E].liftM[({type λ[α] = IterateeT[X, E, F, α]})#λ, A](iter)

  private[iteratee] class FG[F[_[_], _], G[_]] {
    type FGA[A] = F[G, A]
    type IterateeM[A] = IterateeT[X, E, FGA, A]
    type StepM[A] = StepT[X, (E, E), FGA, A]
  }

  trait Value[A] {
    def value: A
  }

  def crossE[G[_]: Monad](e1: EnumeratorP[X, E, G], e2: EnumeratorP[X, E, G]): EnumeratorP[X, (E, E), G] = new EnumeratorP[X, (E, E), G] {
    def apply[F[_[_], _], A](implicit t: MonadTrans[F]): EnumeratorT[X, (E, E), FG[F, G]#FGA, A] = new Value[EnumeratorT[X, (E, E), FG[F, G]#FGA, A]] {
      import t.apply
      type FGA[α] = FG[F, G]#FGA[α]
      type IterateeM[α] = FG[F, G]#IterateeM[α]
      type StepM = FG[F, G]#StepM[A]

      implicit val IterateeTM  = IterateeT.IterateeTMonad[X, E, FGA]
      implicit val IterateeTMT = IterateeT.IterateeTMonadTransT[X, E, F]

      def value = { (step: StepT[X, (E, E), FGA, A]) =>
        val e1t = e1.apply[({type λ[ƒ[_], α] = IterateeT[X, E, ({type λ[σ] = F[ƒ, σ]})#λ, α]})#λ, StepT[X, (E, E), FGA, A]]
        val e2t = e2.apply[F, StepT[X, (E, E), FGA, A]]

        def outerLoop(step: StepT[X,(E, E),this.FGA,A]): IterateeT[X, E, IterateeM, StepT[X, (E, E), FGA, A]] = for {
          leap <- cross1[X, E, FGA, A].apply(step)
          _    <- drop[X, E, IterateeM](1)
          eof  <- isEof[X, E, IterateeM]
          sa   <- if (eof) lift[StepM, FGA](done[X, E, FGA, StepM](leap, eofInput)) else outerLoop(leap)
        } yield sa

        def repeat(s: StepT[X, E, FGA, StepT[X, (E, E), FGA, A]]): IterateeT[X, E, FGA, StepT[X, (E, E), FGA, A]] = e2t(s) >>== repeat

        iterateeT[X, (E, E), FGA, A] {
          ((outerLoop(step) >>== e1t).run(x => iterateeT(err[X, E, FGA, StepT[X, (E, E), FGA, A]](x).value)) >>== repeat)
          .run(x => err[X, (E, E), FGA, A](x).value)
        }
      }
    }.value
  }
}


// vim: set ts=4 sw=4 et:

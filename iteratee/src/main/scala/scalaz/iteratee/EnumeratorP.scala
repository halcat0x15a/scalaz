package scalaz
package iteratee

import Iteratee._
import Enumeratee2T._

abstract class EnumeratorP[X, E, G[_]: Monad] {
  def apply[F[_[_], _], A](implicit t: MonadTrans[F]): EnumeratorT[X, E, ({type λ[α] = F[G, α]})#λ, A]
}

trait EnumeratorPFunctions {
  implicit def enumPStream[X, E, G[_]: Monad](xs : Stream[E]) = new EnumeratorP[X, E, G] {
    def apply[F[_[_],_], A](implicit t: MonadTrans[F]) = {
      implicit val fmt = t.apply
      Iteratee.enumStream[X, E, ({ type λ[α] = F[G, α] })#λ, A](xs)
    }
  }

  def crossE[X, E, G[_]: Monad](e1: EnumeratorP[X, E, G], e2: EnumeratorP[X, E, G]): EnumeratorP[X, (E, E), G] = (new XE[X, E]).crossE[G](e1, e2)

  def cogroupE[X, E: Order, G[_]: Monad](e1: EnumeratorP[X, E, G], e2: EnumeratorP[X, E, G]): EnumeratorP[X, Either3[E, (E, E), E], G] = (new XE[X, E]).cogroupE[G](e1, e2)

  def matchE[X, E: Order, G[_]: Monad](e1 : EnumeratorP[X, E, G], e2 : EnumeratorP[X, E, G]): EnumeratorP[X, (E, E), G] = (new XE[X, E]).matchE[G](e1, e2)

  // this class just exists for partial type application
  private class XE[X, E] {
    import scalaz.syntax.Syntax.bind._
    import scalaz.syntax.Syntax.order._

    // frequently used type lambdas
    private class FG[F[_[_], _], G[_]] {
      type FGA[A] = F[G, A]
      type IterateeM[A] = IterateeT[X, E, FGA, A]
      type StepM[A] = StepT[X, (E, E), FGA, A]
    }

    trait Value[A] {
      def value: A
    }

    private def lift[A, EE, F[_]: Monad](iter: IterateeT[X, EE, F, A]): IterateeT[X, EE, ({type λ[α] = 
      IterateeT[X, EE, F, α]})#λ, A] = IterateeT.IterateeTMonadTrans[X, EE].liftM[({type λ[α] = IterateeT[X, EE, F, α]})#λ, A](iter)

    def crossE[G[_]: Monad](e1: EnumeratorP[X, E, G], e2: EnumeratorP[X, E, G]): EnumeratorP[X, (E, E), G] = new EnumeratorP[X, (E, E), G] {
      def apply[F[_[_], _], A](implicit t: MonadTrans[F]): EnumeratorT[X, (E, E), FG[F, G]#FGA, A] = new Value[EnumeratorT[X, (E, E), FG[F, G]#FGA, A]] {
        import t.apply
        type FGA[α] = FG[F, G]#FGA[α]
        type IterateeM[α] = FG[F, G]#IterateeM[α]
        type StepM = FG[F, G]#StepM[A]

        implicit val IterateeTM  = IterateeT.IterateeTMonad[X, E, FGA]
        implicit val IterateeTMT = IterateeT.IterateeTMonadTransT[X, E, F]

        def value: StepM => IterateeT[X, (E, E), FGA, A] = (step: StepM) => {
          val e1t = e1.apply[F, StepM]
          val e2t = e2.apply[F, StepM]

          def outerLoop(step: StepM): IterateeT[X, E, FGA, StepM] =
            for {
              outerOpt   <- head[X, E, FGA]
              sa         <- outerOpt match {
                              case Some(e) => 
                                val pairingIteratee = EnumerateeT.map[X, E, (E, E), FGA, A]((a: E) => (e, a)).apply(step)
                                val nextStep = (pairingIteratee >>== e2t).run(x => err[X, (E, E), FGA, A](x).value)
                                iterateeT[X, (E, E), FGA, A](nextStep) >>== outerLoop

                              case None    => 
                                done[X, E, FGA, StepM](step, eofInput) 
                            }
            } yield sa

          iterateeT[X, (E, E), FGA, A] {
            (outerLoop(step) >>== e1t).run(x => err[X, (E, E), FGA, A](x).value)
          }
        }
      }.value
    }

    def cogroupE[G[_]](e1 : EnumeratorP[X, E, G], e2 : EnumeratorP[X, E, G])(implicit gm : Monad[G], order : Order[E]) = new EnumeratorP[X, Either3[E, (E, E), E], G] {
      def apply[F[_[_], _], A](implicit t: MonadTrans[F]): EnumeratorT[X, Either3[E, (E, E), E], FG[F, G]#FGA, A] = new Value[EnumeratorT[X, Either3[E, (E, E), E], FG[F, G]#FGA, A]] {

        import Either3._
        import t.apply
        type FGA[α] = FG[F, G]#FGA[α]
        type StepM = StepT[X, Either3[E, (E, E), E], FGA, A]

        val e1t = e1.apply[({ type λ[β[_], α] = IterateeT[X, E, ({ type λ[γ] = F[β, γ] })#λ, α]})#λ, StepM]
        val e2t = e2.apply[F, StepM]

        val value : StepM => IterateeT[X, Either3[E, (E, E), E], FGA, A] = (step : StepM) => {
          iterateeT(((cogroupI[X, E, FGA, A](step) >>== e1t).run(err _) >>== e2t).run(x => t.liftM(gm.point(serr(x)))))
        }
      }.value
    }

    def matchE[G[_]](e1 : EnumeratorP[X, E, G], e2 : EnumeratorP[X, E, G])(implicit gm : Monad[G], order : Order[E]) = new EnumeratorP[X, (E, E), G] {
      def apply[F[_[_], _], A](implicit t: MonadTrans[F]): EnumeratorT[X, (E, E), FG[F, G]#FGA, A] = new Value[EnumeratorT[X, (E, E), FG[F, G]#FGA, A]] {

        import t.apply
        type FGA[α] = FG[F, G]#FGA[α]
        type StepM = StepT[X, (E, E), FGA, A]

        val e1t = e1.apply[({ type λ[β[_], α] = IterateeT[X, E, ({ type λ[γ] = F[β, γ] })#λ, α]})#λ, StepM]
        val e2t = e2.apply[F, StepM]

        val value : StepM => IterateeT[X, (E, E), FGA, A] = (step : StepM) => {
          iterateeT(((matchI[X, E, FGA, A](step) >>== e1t).run(err _) >>== e2t).run(x => t.liftM(gm.point(serr(x)))))
        }
      }.value
    }
  }
}


// vim: set ts=4 sw=4 et:

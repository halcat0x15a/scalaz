package scalaz
package iteratee

import Iteratee._
import Enumeratee2T._

import scalaz.syntax.Syntax.bind._
import scalaz.syntax.Syntax.order._

object EnumeratorP {
  trait ForallM[P[_[_], _]] {
    def apply[F[_]: Monad, A]: P[F, A]
  }
}

abstract class EnumeratorP[X, E, G[_]: Monad] {
  def apply[F[_[_], _], A](implicit t: MonadTrans[F]): EnumeratorT[X, E, ({type λ[α] = F[G, α]})#λ, A]
}

trait EnumeratorPFunctions {
  import EnumeratorP._
  private trait Value[A] {
    def value: A
  }

  implicit def enumPStream[X, E, G[_]: Monad](xs : Stream[E]) = new EnumeratorP[X, E, G] {
    def apply[F[_[_],_], A](implicit t: MonadTrans[F]) = {
      implicit val fmt = t.apply
      Iteratee.enumStream[X, E, ({ type λ[α] = F[G, α] })#λ, A](xs)
    }
  }

  def crossE[X, E, G[_]: Monad](e1: EnumeratorP[X, E, G], e2: EnumeratorP[X, E, G]): EnumeratorP[X, (E, E), G] = new EnumeratorP[X, (E, E), G] {
    def apply[F[_[_], _], A](implicit t: MonadTrans[F]): EnumeratorT[X, (E, E), ({type λ[α] = F[G, α]})#λ, A] = new Value[EnumeratorT[X, (E, E), ({type λ[α] = F[G, α]})#λ, A]] {
      import t.apply
      type FG[α] = F[G, α]
      type IterateeM[α] = IterateeT[X, E, FG, A]
      type StepM = StepT[X, (E, E), FG, A]

      implicit val IterateeTM  = IterateeT.IterateeTMonad[X, E, FG]
      implicit val IterateeTMT = IterateeT.IterateeTMonadTransT[X, E, F]

      def value: StepM => IterateeT[X, (E, E), FG, A] = (step: StepM) => {
        val e1t = e1.apply[F, StepM]
        val e2t = e2.apply[F, StepM]

        def outerLoop(step: StepM): IterateeT[X, E, FG, StepM] =
          for {
            outerOpt   <- head[X, E, FG]
            sa         <- outerOpt match {
                            case Some(e) => 
                              val pairingIteratee = EnumerateeT.map[X, E, (E, E), FG, A]((a: E) => (e, a)).apply(step)
                              val nextStep = (pairingIteratee >>== e2t).run(x => err[X, (E, E), FG, A](x).value)
                              iterateeT[X, (E, E), FG, A](nextStep) >>== outerLoop

                            case None    => 
                              done[X, E, FG, StepM](step, eofInput) 
                          }
          } yield sa

        iterateeT[X, (E, E), FG, A] {
          (outerLoop(step) >>== e1t).run(x => err[X, (E, E), FG, A](x).value)
        }
      }
    }.value
  }

  def liftE[X, O, I, G[_]: Monad](e2t: ForallM[({type λ[β[_], α] = Enumeratee2T[X, O, I, β, α]})#λ]): (EnumeratorP[X, O, G], EnumeratorP[X, O, G]) => EnumeratorP[X, I, G] = {
    (e1: EnumeratorP[X, O, G], e2: EnumeratorP[X, O, G]) => new EnumeratorP[X, I, G] {
      def apply[F[_[_], _], A](implicit t: MonadTrans[F]): EnumeratorT[X, I, ({type λ[α] = F[G, α]})#λ, A] = {
        new Value[EnumeratorT[X, I, ({type λ[α] = F[G, α]})#λ, A]] {
          import t.apply
          type FG[α] = F[G, α]
          type StepM = StepT[X, I, FG, A]

          val enum1 = e1.apply[({ type λ[β[_], α] = IterateeT[X, O, ({ type λ[γ] = F[β, γ] })#λ, α]})#λ, StepM]
          val enum2 = e2.apply[F, StepM]

          val value : StepM => IterateeT[X, I, FG, A] = (step : StepM) => {
            iterateeT(((e2t[FG, A].apply(step) >>== enum1).run(err _) >>== enum2).run(x => t.liftM(implicitly[Monad[G]].point(serr(x)))))
          }
        }.value
      }
    }
  }

  def cogroupE[X, E: Order, G[_]: Monad] = liftE[X, E, Either3[E, (E, E), E], G] {
    new ForallM[({type λ[β[_], α] = Enumeratee2T[X, E, Either3[E, (E, E), E], β, α]})#λ] {
      def apply[F[_]: Monad, A] = cogroupI[X, E, F, A]
    }
  }

  def matchE[X, E: Order, G[_]: Monad] = liftE[X, E, (E, E), G] { 
    new ForallM[({type λ[β[_], α] = Enumeratee2T[X, E, (E, E), β, α]})#λ] {
      def apply[F[_]: Monad, A] = matchI[X, E, F, A]
    }
  }

  def mergeE[X, E: Order, G[_]: Monad] = liftE[X, E, E, G] { 
    new ForallM[({type λ[β[_], α] = Enumeratee2T[X, E, E, β, α]})#λ] {
      def apply[F[_]: Monad, A] = mergeI[X, E, F, A]
    }
  }

  def mergeDistinctE[X, E: Order, G[_]: Monad] = liftE[X, E, E, G] { 
    new ForallM[({type λ[β[_], α] = Enumeratee2T[X, E, E, β, α]})#λ] {
      def apply[F[_]: Monad, A] = mergeDistinctI[X, E, F, A]
    }
  }
}


// vim: set ts=4 sw=4 et:

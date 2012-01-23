package scalaz
package iteratee

import Iteratee._
import Enumeratee2T._

import scalaz.syntax.Syntax.bind._
import scalaz.syntax.Syntax.order._

trait ForallM[P[_[_], _]] {
  def apply[F[_]: Monad, A]: P[F, A]
}

abstract class EnumeratorP[X, E, G[_]: Monad] { self =>
  def apply[F[_[_], _]](implicit t: MonadTrans[F]): EnumeratorT[X, E, ({type λ[α] = F[G, α]})#λ]
  /*

  def mapE[I](enumerateeT: ForallM[({type λ[β[_], α] = EnumerateeT[X, E, I, β, α]})#λ]): EnumeratorP[X, I, G] = new EnumeratorP[X, I, G] {
    def apply[F[_[_], _]](implicit T: MonadTrans[F]): EnumeratorT[X, I, ({type λ[α] = F[G, α]})#λ] = {
      type FG[α] = F[G, α]
      implicit val FGM = T[G]
      new EnumeratorT[X, I, FG] {
        def apply[A](step: StepT[X, I, FG, A]) = iterateeT((enumerateeT[FG, A].apply(step) &= self[F]).run(x => err[X, I, FG, A](x).value))
      }
    }
  }
  */
}

trait EnumeratorPFunctions {
  implicit def enumPStream[X, E, G[_]: Monad](xs : Stream[E]): EnumeratorP[X, E, G] = new EnumeratorP[X, E, G] {
    def apply[F[_[_],_]](implicit T: MonadTrans[F]): EnumeratorT[X, E, ({type λ[α] = F[G, α]})#λ] = {
      type FG[α] = F[G, α]
      implicit val FGM = T[G]
      Iteratee.enumStream[X, E, FG](xs)
    }
  }

  def crossE[X, E, G[_]: Monad](e1: EnumeratorP[X, E, G], e2: EnumeratorP[X, E, G]): EnumeratorP[X, (E, E), G] = new EnumeratorP[X, (E, E), G] {
    def apply[F[_[_], _]](implicit T: MonadTrans[F]): EnumeratorT[X, (E, E), ({type λ[α] = F[G, α]})#λ] = {
      type FG[α] = F[G, α]
      type StepM[α] = StepT[X, (E, E), FG, α]
      type IterateeM[α] = IterateeT[X, (E, E), FG, α]

      implicit val FGM = T[G]
      implicit val IterateeTM  = IterateeT.IterateeTMonad[X, E, FG]
      implicit val IterateeTMT = IterateeT.IterateeTMonadTransT[X, E, F]

      new EnumeratorT[X, (E, E), FG] {
        def apply[A] = (step: StepT[X, (E, E), FG, A]) => {
          val e1t = e1[F]
          val e2t = e2[F]

          def outerLoop(step: StepM[A]): IterateeT[X, E, FG, StepM[A]] =
            for {
              outerOpt   <- head[X, E, FG]
              sa         <- outerOpt match {
                              case Some(e) => 
                                val pairingIteratee = EnumerateeT.map[X, E, (E, E), FG]((a: E) => (e, a)).apply(step)
                                val nextStep = (pairingIteratee &= e2t).run(x => err[X, (E, E), FG, A](x).value)
                                iterateeT[X, (E, E), FG, A](nextStep) >>== outerLoop

                              case None    => 
                                done[X, E, FG, StepM[A]](step, eofInput) 
                            }
            } yield sa

          iterateeT[X, (E, E), FG, A] {
            (outerLoop(step) &= e1t).run(x => err[X, (E, E), FG, A](x).value)
          }
        }
      }
    }
  }

  def liftE2[X, O, I, G[_]: Monad](e2t: ForallM[({type λ[β[_], α] = Enumeratee2T[X, O, I, β, α]})#λ]): (EnumeratorP[X, O, G], EnumeratorP[X, O, G]) => EnumeratorP[X, I, G] = {
    (e1: EnumeratorP[X, O, G], e2: EnumeratorP[X, O, G]) => new EnumeratorP[X, I, G] {
      def apply[F[_[_], _]](implicit T: MonadTrans[F]): EnumeratorT[X, I, ({type λ[α] = F[G, α]})#λ] = {
        type FG[α] = F[G, α]
        implicit val FGM = T[G]

        new EnumeratorT[X, I, FG] {
          def apply[A] = (step: StepT[X, I, FG, A]) => {
            val enum1 = e1[({ type λ[β[_], α] = IterateeT[X, O, ({ type λ[γ] = F[β, γ] })#λ, α]})#λ]
            val enum2 = e2[F]

            iterateeT(((e2t[FG, A].apply(step) &= enum1).run(err _) &= enum2).run(x => T.liftM(Monad[G].point(serr(x)))))
          }
        }
      }
    }
  }

  def cogroupE[X, E: Order, G[_]: Monad] = liftE2[X, E, Either3[E, (E, E), E], G] {
    new ForallM[({type λ[β[_], α] = Enumeratee2T[X, E, Either3[E, (E, E), E], β, α]})#λ] {
      def apply[F[_]: Monad, A] = cogroupI[X, E, F, A]
    }
  }

  def matchE[X, E: Order, G[_]: Monad] = liftE2[X, E, (E, E), G] { 
    new ForallM[({type λ[β[_], α] = Enumeratee2T[X, E, (E, E), β, α]})#λ] {
      def apply[F[_]: Monad, A] = matchI[X, E, F, A]
    }
  }

  def mergeE[X, E: Order, G[_]: Monad] = liftE2[X, E, E, G] { 
    new ForallM[({type λ[β[_], α] = Enumeratee2T[X, E, E, β, α]})#λ] {
      def apply[F[_]: Monad, A] = mergeI[X, E, F, A]
    }
  }

  def mergeDistinctE[X, E: Order, G[_]: Monad] = liftE2[X, E, E, G] { 
    new ForallM[({type λ[β[_], α] = Enumeratee2T[X, E, E, β, α]})#λ] {
      def apply[F[_]: Monad, A] = mergeDistinctI[X, E, F, A]
    }
  }
}

object EnumeratorP extends EnumeratorPFunctions


// vim: set ts=4 sw=4 et:

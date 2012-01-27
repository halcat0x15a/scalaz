package scalaz
package iteratee

import Iteratee._
import Enumeratee2T._

import scalaz.syntax.Syntax.bind._
import scalaz.syntax.Syntax.order._

trait ForallM[P[_[_]]] {
  def apply[F[_]: Monad]: P[F]
}

abstract class EnumeratorP[X, E, F[_]] { self =>
  def apply[G[_]](implicit ord: MonadPartialOrder[G, F]): EnumeratorT[X, E, G]
}

trait EnumeratorPFunctions {
  def enumPStream[X, E, F[_]: Monad](xs : Stream[E]): EnumeratorP[X, E, F] = new EnumeratorP[X, E, F] {
    def apply[G[_]](implicit ord: MonadPartialOrder[G, F]): EnumeratorT[X, E, G] = {
      import ord._
      Iteratee.enumStream[X, E, G](xs)
    }
  }

  def liftE2[X, O, I, F[_]](e2t: ForallM[({type λ[β[_]] = Enumeratee2T[X, O, I, β]})#λ]): (EnumeratorP[X, O, F], EnumeratorP[X, O, F]) => EnumeratorP[X, I, F] = {
    (e1: EnumeratorP[X, O, F], e2: EnumeratorP[X, O, F]) => new EnumeratorP[X, I, F] {
      def apply[G[_]](implicit ord: MonadPartialOrder[G, F]): EnumeratorT[X, I, G] = 
        new EnumeratorT[X, I, G] {
          import ord._
          implicit val IOrd = ord.transform[({ type λ[β[_], α] = IterateeT[X, O, β, α] })#λ]
          def apply[A] = {
            val enum1 = e1[({ type λ[α] = IterateeT[X, O, G, α]})#λ]
            val enum2 = e2[G]
            (step: StepT[X, I, G, A]) => iterateeT(((e2t[G].apply(step) &= enum1).run(err _) &= enum2).run(x => ord.promote(Monad[F].point(serr(x)))))
          }
        }
    }
  }

  def cogroupE[X, E: Order, F[_]: Monad] = liftE2[X, E, Either3[E, (E, E), E], F] {
    new ForallM[({type λ[β[_]] = Enumeratee2T[X, E, Either3[E, (E, E), E], β]})#λ] {
      def apply[G[_]: Monad] = cogroupI[X, E, G]
    }
  }

  def matchE[X, E: Order, F[_]: Monad] = liftE2[X, E, (E, E), F] { 
    new ForallM[({type λ[β[_]] = Enumeratee2T[X, E, (E, E), β]})#λ] {
      def apply[G[_]: Monad] = matchI[X, E, G]
    }
  }

  def mergeE[X, E: Order, F[_]: Monad] = liftE2[X, E, E, F] { 
    new ForallM[({type λ[β[_]] = Enumeratee2T[X, E, E, β]})#λ] {
      def apply[G[_]: Monad] = mergeI[X, E, G]
    }
  }

  def mergeDistinctE[X, E: Order, F[_]: Monad] = liftE2[X, E, E, F] { 
    new ForallM[({type λ[β[_]] = Enumeratee2T[X, E, E, β]})#λ] {
      def apply[G[_]: Monad] = mergeDistinctI[X, E, G]
    }
  }
}

// vim: set ts=4 sw=4 et:

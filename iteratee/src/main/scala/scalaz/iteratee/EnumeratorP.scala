package scalaz
package iteratee

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
}


// vim: set ts=4 sw=4 et:

package scalaz

final class IdT[F[_], X](val value: F[X])

private class IdTMonad[F[_]: Monad] extends Monad[({type λ[α] = IdT[F, α]})#λ] {
  def point[A](a: => A) = IdT[F, A](implicitly[Monad[F]].point(a))

  def bind[A, B](fa: IdT[F, A])(f: A => IdT[F, B]) = 
    IdT[F, B](implicitly[Monad[F]].bind(fa.value)(f andThen ((_: IdT[F, B]).value)))
}

trait IdTInstances {
  implicit def idTMonadTrans: MonadTrans[IdT] = IdTMonadTrans
  implicit def idTMonad[F[_]: Monad]: Monad[({type λ[α] = IdT[F, α]})#λ] = new IdTMonad[F]
}

private object IdTMonadTrans extends MonadTrans[IdT] {
  def liftM[G[_], A](a: G[A])(implicit G: Monad[G]): IdT[G, A] = IdT[G, A](a)

  def hoist[M[_]: Monad, N[_]](f: M ~> N) = new (({type f[x] = IdT[M, x]})#f ~> ({type f[x] = IdT[N, x]})#f) {
    def apply[A](fa: IdT[M, A]): IdT[N, A] = IdT[N, A](f.apply(fa.value))
  }

  implicit def apply[G[_] : Monad]: Monad[({type λ[α] = IdT[G, α]})#λ] = IdT.idTMonad[G]
}

// vim: set ts=4 sw=4 et:

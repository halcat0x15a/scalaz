package scalaz

trait IdT[F[_], X] {
  def value: F[X]
}

private class IdTMonad[F[_]: Monad] extends Monad[({type λ[α] = IdT[F, α]})#λ] {
  def point[A](a: => A) = new IdT[F, A] { val value = implicitly[Monad[F]].point(a) }
  def bind[A, B](fa: IdT[F, A])(f: A => IdT[F, B]) = new IdT[F, B] { 
    def value = implicitly[Monad[F]].bind(fa.value)(f andThen ((_: IdT[F, B]).value))
  }
}

trait IdTInstances {
  implicit def idTMonadTrans: MonadTrans[IdT] = IdTMonadTrans
  implicit def idTMonad[F[_]: Monad]: Monad[({type λ[α] = IdT[F, α]})#λ] = new IdTMonad[F]
}

private object IdTMonadTrans extends MonadTrans[IdT] {
  def liftM[G[_], A](a: G[A])(implicit G: Monad[G]): IdT[G, A] = new IdT[G, A] {
    val value = a
  }

  def hoist[M[_]: Monad, N[_]](f: M ~> N) = new (({type f[x] = IdT[M, x]})#f ~> ({type f[x] = IdT[N, x]})#f) {
    def apply[A](fa: IdT[M, A]): IdT[N, A] = new IdT[N, A] {
      val value = f.apply(fa.value)
    }
  }

  implicit def apply[G[_] : Monad]: Monad[({type λ[α] = IdT[G, α]})#λ] = IdT.idTMonad[G]
}

// vim: set ts=4 sw=4 et:

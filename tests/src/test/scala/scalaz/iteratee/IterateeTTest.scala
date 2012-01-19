package scalaz
package iteratee

import std.AllInstances._
import Iteratee._
import effect._

class IterateeTTest extends Spec {

  "head" in {
    (head[Unit, Int, Id].>>==(enumStream(Stream(1, 2, 3)))).runOrZero must be_===(Some(1))

    (head[Unit, Int, Id] >>== Stream(1, 2, 3)).runOrZero must be_===(Some(1))
  }

  "consume" in {
    (consume[Unit, Int, Id, List] >>== Stream(1, 2, 3)).runOrZero must be_===(List(1, 2, 3))
  }

  "match equal pairs" in {
    implicit val v = IterateeT.IterateeTMonad[Unit, Int, Id]
    val enum  = enumStream[Unit, Int, ({type L[A] = IterateeT[Unit, Int, Id, A]})#L, List[(Int, Int)]](Stream(1, 3, 5, 7)) 
    val enum2 = enumStream[Unit, Int, Id, List[(Int, Int)]](Stream(2, 3, 4, 5, 6)) 

    ((matchI(consume[Unit, (Int, Int), Id, List].value) >>== enum).run(_ => done(Nil, eofInput)) >>== enum2).run(_ => Nil) must_== List((3, 3), (5, 5))
  }

  "cogroup" in {
    import Either3._
    import IdT._
    implicit val ls = listShow[Either3[Int, (Int, Int), Int]]
    implicit val v = IterateeT.IterateeTMonad[Unit, Int, Id]
    implicit val idtm = IdT.idTMonad[Id]
    val enum  = enumPStream[Unit, Int, Id](Stream(1, 3, 5, 7)) 
    val enum2 = enumPStream[Unit, Int, Id](Stream(2, 3, 4, 5, 6)) 

    (consume[Unit, Either3[Int, (Int, Int), Int], ({ type λ[α] = IdT[Id,α] })#λ, List] >>== cogroup(enum, enum2).apply[IdT, List[Either3[Int, (Int, Int), Int]]]).run(_ => idtm.point(Nil)).value must be_===(List[Either3[Int, (Int, Int), Int]](
      left3(1),
      right3(2),
      middle3((3, 3)),
      right3(4),
      middle3((5, 5)),
      right3(6),
      left3(7)
    ))
  }

/*
  "merge sorted iteratees" in {
    implicit val v = IterateeT.IterateeTMonad[Unit, Int, Id]
    val enum  = enumStream[Unit, Int, ({type L[A] = IterateeT[Unit, Int, Id, A]})#L, List[Int]](Stream(1, 3, 5)) 
    val enum2 = enumStream[Unit, Int, Id, List[Int]](Stream(2, 3, 4, 5, 6)) 

    ((mergeI(consume[Unit, Int, Id, List].value) >>== enum).run(_ => done(Nil, eofInput)) >>== enum2).run(_ => Nil) must_== List(1, 2, 3, 3, 4, 5, 5, 6)
  }
  */

  "cross the first element with all of the second iteratee's elements" in {
    import IdT._
    implicit val v = IterateeT.IterateeTMonad[Unit, Int, Id]
    val enum1p = new EnumeratorP[Unit, Int, Id] {
      def apply[F[_[_], _], A](implicit t: MonadTrans[F]): EnumeratorT[Unit, Int, ({type λ[α] = F[Id, α]})#λ, A] = {
        implicit val fmonad = t.apply[Id]
        enumStream[Unit, Int, ({type λ[α] = F[Id, α]})#λ, A](Stream(1, 3, 5)) 
      }
    }

    val enum2p = new EnumeratorP[Unit, Int, Id] {
      def apply[F[_[_], _], A](implicit t: MonadTrans[F]): EnumeratorT[Unit, Int, ({type λ[α] = F[Id, α]})#λ, A] = {
        implicit val fmonad = t.apply[Id]
        enumStream[Unit, Int, ({type λ[α] = F[Id, α]})#λ, A](Stream(2, 3, 4)) 
      }
    }

    implicit val idTm = idTMonad[Id]
    val consumer = consume[Unit, (Int, Int), ({type λ[α] = IdT[Id, α]})#λ, List]
    val producer = cross[Unit, Int, Id](enum1p, enum2p).apply[IdT, List[(Int, Int)]]
    (consumer >>== producer).run(x => sys.error("...")).value must be_===(List(
      (1, 2), (1, 3), (1, 4), (3, 2), (3, 3), (3, 4), (5, 2), (5, 3), (5, 4)
    ))
  }

  object instances {
    object iterateet {
      def monad[F[_]: Monad, X, E] = Monad[({type λ[α] = IterateeT[X, E, F, α]})#λ]
      def liftIO[F[_]: MonadIO, X, E] = LiftIO[({type λ[α] = IterateeT[X, E, F, α]})#λ]
      def monadIO[F[_]: MonadIO, X, E] = MonadIO[({type λ[α] = IterateeT[X, E, F, α]})#λ]
    }

    object iteratee {
      def monad[X, E, F] = Monad[({type λ[α] = Iteratee[X, E, α]})#λ]
    }

    object enumeratorT {
      def semigroup[X, E, F[_]: Bind, A] = Semigroup[EnumeratorT[X, E, F, A]]
      def monoid[X, E, F[_]: Monad, A] = Monoid[EnumeratorT[X, E, F, A]]
      def plus[X, E, F[_]: Bind, A] = Plus[({type λ[α]=EnumeratorT[X, E, F, α]})#λ]
      def empty[X, E, F[_]: Monad, A] = PlusEmpty[({type λ[α]=EnumeratorT[X, E, F, α]})#λ]
    }

    object enumerator {
      def monoid[X, E, A] = Monoid[Enumerator[X, E, A]]
    }
  }
}

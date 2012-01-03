package scalaz
package iteratee

import effect._
import scalaz.syntax.Syntax.bind._
import scalaz.syntax.Syntax.order._

import Iteratee._

trait EnumeratorTInstances0 {
  implicit def enumeratorTSemigroup[X, E, F[_], A](implicit F0: Bind[F]): Semigroup[EnumeratorT[X, E, F, A]] = new EnumeratorTSemigroup[X, E, F, A] {
    implicit def F = F0
  }
}

trait EnumeratorTInstances extends EnumeratorTInstances0 {
  implicit def enumeratorTMonoid[X, E, F[_], A](implicit F0: Monad[F]): Monoid[EnumeratorT[X, E, F, A]] = new EnumeratorTMonoid[X, E, F, A] {
    implicit def F = F0
  }
  implicit def enumeratorMonoid[X, E, A]: Monoid[Enumerator[X, E, A]] = new Monoid[Enumerator[X, E, A]] {
    def append(f1: (Step[X, E, A]) => Step[X, E, A], f2: => (Step[X, E, A]) => Step[X, E, A]): Step[X, E, A] => Step[X, E, A] = f1 andThen f2
    def zero: (Step[X, E, A]) => Step[X, E, A] = x => x
  }
}

trait EnumeratorTFunctions {
  def enumEofT[X, E, F[_] : Monad, A](e: (=> X) => IterateeT[X, E, F, A]): EnumeratorT[X, E, F, A] =
    j => {
      j.fold(
        cont = k =>
          k(eofInput) >>== (s =>
            s >-(
              sys.error("diverging iteratee")
              , enumEofT(e) apply s
              , enumEofT(e) apply s
              ))
        , done = (a, _) =>
          StepT.sdone[X, E, F, A](a, eofInput).pointI
        , err = e(_)
      )
    }

  def enumerate[A, O](as: Stream[A]): Enumerator[Unit, A, O] =
    i =>
      as match {
        case Stream.Empty => i
        case x #:: xs     =>
          i.fold(done = (_, _) => i, cont = k => enumerate(xs)(k(elInput(x)).value), err = e => err[Unit, A, Id, O](e).value)
      }

  implicit def enumStream[X, E, F[_] : Monad, A](xs: Stream[E]): EnumeratorT[X, E, F, A] = {
    s =>
      xs match {
        case h #:: t => s.mapContOr(_(elInput(h)) >>== enumStream(t), s.pointI)
        case _       => s.pointI
      }
  }

  implicit def enumIterator[X, E, A](x: Iterator[E]): EnumeratorT[X, E, IO, A] = {
    def loop: EnumeratorT[X, E, IO, A] = {
      s =>
        s.mapContOr(
          k =>
            if (x.hasNext) {
              val n = x.next
              k(elInput(n)) >>== loop
            } else s.pointI
          , s.pointI
        )
    }
    loop
  }

  import java.io._

  implicit def enumReader[X, A](r: Reader): EnumeratorT[X, IoExceptionOr[Char], IO, A] = {
    def loop: EnumeratorT[X, IoExceptionOr[Char], IO, A] = {
      s =>
        s.mapContOr(
          k => {
            val i = IoExceptionOr(r.read)
            if (i exists (_ != -1)) k(elInput(i.map(_.toChar))) >>== loop
            else s.pointI
          }
          , s.pointI
        )
    }
    loop
  }

  def checkCont0[X, E, F[_], A](z: EnumeratorT[X, E, F, A] => (Input[E] => IterateeT[X, E, F, A]) => IterateeT[X, E, F, A])(implicit p: Pointed[F]): EnumeratorT[X, E, F, A] = {
    def step: EnumeratorT[X, E, F, A] = {
      s =>
        s.mapContOr(
          k => z(step)(k)
          , s.pointI
        )
    }
    step
  }

  def checkCont1[S, X, E, F[_], A](z: (S => EnumeratorT[X, E, F, A]) => S => (Input[E] => IterateeT[X, E, F, A]) => IterateeT[X, E, F, A], t: S)(implicit p: Pointed[F]): EnumeratorT[X, E, F, A] = {
    def step: S => EnumeratorT[X, E, F, A] = {
      o => s =>
        s.mapContOr(
          k => z(step)(o)(k)
          , s.pointI
        )
    }
    step(t)
  }

  def iterate[X, E, F[_] : Monad, A](f: E => E, e: E): EnumeratorT[X, E, F, A] = {
    checkCont1[E, X, E, F, A](s => t => k => k(elInput(e)) >>== s(f(t)), e)
  }

  def repeat[X, E, F[_] : Monad, A](e: E): EnumeratorT[X, E, F, A] = {
    checkCont0[X, E, F, A](s => k => k(elInput(e)) >>== s)
  }

  def doneOr[X, O, I, F[_] : Pointed, A](f: (Input[I] => IterateeT[X, I, F, A]) => IterateeT[X, O, F, StepT[X, I, F, A]]): EnumerateeT[X, O, I, F, A] = {
    s =>
      def d: IterateeT[X, O, F, StepT[X, I, F, A]] = done(s, emptyInput)
      s.fold(
        cont = k => f(k)
        , done = (_, _) => d
        , err = _ => d
      )
  }

/*
  type AChunk[F[_], A] = (A, F[A])
  def merge[X, E: Order, F[_]: Monad, A]: EnumerateeT[X, E, E, ({type λ[α] = IterateeT[X, E, F, α]})#λ, A] = {
    val FMonad = implicitly[Monad[F]]
    def loop(f: Input[E] => F[(IterateeT[X, E, F, A], Input[E])], 
             c1: AChunk[({type λ[α] = IterateeT[X, E, F, α]})#λ, E],
             c2: AChunk[({type λ[α] = IterateeT[X, E, F, α]})#λ, E]) : IterateeT[X, E, F, IterateeT[X, E, F, A]] = {
      sys.error("todo")
    }

    (s: StepT[X, E, ({type λ[α] = IterateeT[X, E, F, α]})#λ, A]) => s.mapContOr(
      ik => sys.error("todo"), //loop(ik, sys.error("todo"), sys.error("todo")),
      IterateeT.IterateeTMonadTrans[X, E].liftM[({type λ[α] = IterateeT[X, E, F, α]})#λ, StepT[X, E, ({type λ[α] = IterateeT[X, E, F, α]})#λ, A]]{
        implicitly[Pointed[({type λ[α] = IterateeT[X, E, ({type λ[α] = IterateeT[X, E, F, α]})#λ, α]})#λ]].point(s)
        //done(s, eofInput[E]) 
      } : IterateeT[X, E, ({type λ[α] = IterateeT[X, E, F, α]})#λ, StepT[X, E, ({type λ[α] = IterateeT[X, E, F, α]})#λ, A]]
    )
  }
*/

  private class NestedIterateeOps[X, E, F[_]: Monad] {
    type IterateeM[A] = IterateeT[X, E, F, A]
    
    def lift[A](iter: IterateeT[X, E, F, A]): IterateeT[X, E, IterateeM, A] = IterateeT.IterateeTMonadTrans[X, E].liftM[({type λ[α] = IterateeT[X, E, F, α]})#λ, A](iter)

    def end[A](step: StepT[X, (E, E), F, A]): IterateeT[X, E, F, A] = {
      step.fold(
        cont = contf  => IterateeT(contf(eofInput).value >>= (s => end(s).value)),
        done = (a, r) => done(a, emptyInput),
        err  = e      => err(e)
      )
    }

    def matchI[A](step: StepT[X, (E, E), F, A])(implicit order: Order[E]): IterateeT[X, E, IterateeM, A] = {
      step.fold[IterateeT[X, E, IterateeM, A]](
        cont = contf => {
          for {
            leftOpt <- head[X, E, IterateeM]

            rightOpt <- lift(leftOpt.map { left =>
                          for {
                            _        <- dropWhile[X, E, F](_ < left)
                            rightOpt <- peek[X, E, F]
                          } yield rightOpt
                        }.getOrElse(done[X, E, F, Option[E]](None, emptyInput)))
            
            val leftRightOpt = for { left <- leftOpt; right <- rightOpt } yield (left, right)

            a <- leftRightOpt match {
              case Some((left, right)) if (left ?|? right == Ordering.EQ) => 
                val joined = contf(elInput((left, right))).value >>= (s => matchI(s).value.value)
                
                IterateeT[X, E, IterateeM, A](IterateeT(joined))

              case Some(_) => matchI(step)

              case None => lift(end(step))
            }
          } yield a
        },
        done = (a, r) => done(a, emptyInput),
        err  = e => err(e)
      )
    }
    
    def cogroupI[A](step: StepT[X, Either[Either[E, E], (E, E)], F, A])(implicit order: Order[E]): IterateeT[X, E, IterateeM, A] = {
      null
    }
  }
  
  def matchI[X, E: Order, F[_]: Monad, A](step: StepT[X, (E, E), F, A]) = new NestedIterateeOps().matchI(step)
  
  def cogroupI[X, E: Order, F[_]: Monad, A](step: StepT[X, Either[Either[E, E], (E, E)], F, A]) = new NestedIterateeOps().cogroupI(step)
  
  sealed trait EnumeratorTFactory[X, E] {
    def apply[F[_]: Monad, A]: EnumeratorT[X, E, F, A]
  }
  
  def matchE[X, E: Order](enum1F: EnumeratorTFactory[X, E], enum2F: EnumeratorTFactory[X, E]): EnumeratorTFactory[X, (E, E)] = new EnumeratorTFactory[X, (E, E)] {
    def apply[F[_]: Monad, A]: EnumeratorT[X, (E, E), F, A] = {
      val enum1: EnumeratorT[X, E, ({type L[A] = IterateeT[X, E, F, A]})#L, A] = enum1F.apply[({type L[A] = IterateeT[X, E, F, A]})#L, A]
      def enum2[A]: EnumeratorT[X, E, F, A] = enum2F.apply[F, A]
      
      (step: StepT[X, (E, E), F, A]) => {
        val nested = matchI(step)
      
        
      
        null: IterateeT[X, (E, E), F, A]
      }
    }
  }

  
  /*
  Full types for recursive case:
  
    val m3: F[StepT[X, E, F, StepT[X, E, IterateeM, A]]] = contf(elInput((left, right))).value >>= { step: StepT[X, (E, E), F, A] =>
      val m1: IterateeT[X, E, IterateeM, A] = matchE(step)
      val m2: IterateeM[StepT[X, E, IterateeM, A]] = m1.value
      val m3: F[StepT[X, E, F, StepT[X, E, IterateeM, A]]] = m2.value
    
      m3
    }
  
    val m2: IterateeM[StepT[X, E, IterateeM, A]] = IterateeT(m3)
  
    val m1: IterateeT[X, E, IterateeM, A] = IterateeT(m2)
  
    m1
  */
}

//
// Type class implementation traits
//

private[scalaz] trait EnumeratorTSemigroup[X, E, F[_], A] extends Semigroup[EnumeratorT[X, E, F, A]] {
  implicit def F: Bind[F]

  def append(f1: (StepT[X, E, F, A]) => IterateeT[X, E, F, A],
             f2: => (StepT[X, E, F, A]) => IterateeT[X, E, F, A]): (StepT[X, E, F, A]) => IterateeT[X, E, F, A] =
    s => f1(s) >>== f2
}

private[scalaz] trait EnumeratorTMonoid[X, E, F[_], A] extends Monoid[EnumeratorT[X, E, F, A]] with EnumeratorTSemigroup[X, E, F, A] {
  implicit def F: Monad[F]

  def zero: (StepT[X, E, F, A]) => IterateeT[X, E, F, A] = _.pointI
}

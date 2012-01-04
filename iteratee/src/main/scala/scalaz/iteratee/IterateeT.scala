package scalaz
package iteratee

import effect._
import Iteratee._

sealed trait IterateeT[X, E, F[_], A] {
  def value: F[StepT[X, E, F, A]]

  def foldT[Z](
                cont: (Input[E] => IterateeT[X, E, F, A]) => F[Z]
                , done: (=> A, => Input[E]) => F[Z]
                , err: (=> X) => F[Z]
                )(implicit F: Bind[F]): F[Z] =
    F.bind(value)((s: StepT[X, E, F, A]) => s(cont, done, err))

  /** An alias for `run` */
  def apply(e: (=> X) => F[A])(implicit F: Monad[F]): F[A] = run(e)

  /** Run this iteratee */
  def run(e: (=> X) => F[A])(implicit F: Monad[F]): F[A] = {
    val lifte: (=> X) => IterateeT[X, E, F, A] = x => MonadTrans[({type λ[α[_], β] = IterateeT[X, E, α, β]})#λ].liftM(e(x))
    F.bind(>>==(enumEofT(lifte)).value)((s: StepT[X, E, F, A]) => s.fold(
      cont = _ => sys.error("diverging iteratee")
      , done = (a, _) => F.point(a)
      , err = e
    ))
  }

  /** Like `run`, but uses `Monoid[F[A]].zero` in the error case */
  def runOrZero(implicit F: Monad[F], FA: Monoid[F[A]]): F[A] = apply(_ => FA.zero)

  def flatMap[B](f: A => IterateeT[X, E, F, B])(implicit F: Monad[F]): IterateeT[X, E, F, B] = {
    def through(x: IterateeT[X, E, F, A]): IterateeT[X, E, F, B] =
      iterateeT(
        F.bind(x.value)((s: StepT[X, E, F, A]) => s.fold[F[StepT[X, E, F, B]]](
          cont = k => F.point(StepT.scont(u => through(k(u))))
          , done = (a, i) =>
            if (i.isEmpty)
              f(a).value
            else
              F.bind(f(a).value)(_.fold(
                cont = kk => kk(i).value
                , done = (aa, _) => F.point(StepT.sdone[X, E, F, B](aa, i))
                , err = ee => F.point(StepT.serr[X, E, F, B](ee))
              ))
          , err = e => F.point(StepT.serr(e))
        )))
    through(this)
  }

  def map[B](f: A => B)(implicit m: Monad[F]): IterateeT[X, E, F, B] = {
    flatMap(a => StepT.sdone[X, E, F, B](f(a), emptyInput).pointI)
  }

  def >>==[B, C](f: StepT[X, E, F, A] => IterateeT[X, B, F, C])(implicit m: Bind[F]): IterateeT[X, B, F, C] =
    iterateeT(m.bind(value)((s: StepT[X, E, F, A]) => f(s).value))

  def mapI[G[_]](f: F ~> G)(implicit F: Functor[F]): IterateeT[X, E, G, A] = {
    def step: StepT[X, E, F, A] => StepT[X, E, G, A] =
      _.fold(
        cont = k => scont[X, E, G, A](k andThen loop)
        , done = (a, i) => sdone[X, E, G, A](a, i)
        , err = e => serr[X, E, G, A](e)
      )
    def loop: IterateeT[X, E, F, A] => IterateeT[X, E, G, A] = i => iterateeT(f(F.map(i.value)(step)))
    loop(this)
  }

  def up[G[_]](implicit G: Pointed[G], F: Functor[F], FC: CoPointed[F]): IterateeT[X, E, G, A] = {
    mapI(new (F ~> G) {
      def apply[A](a: F[A]) = G.point(FC.copoint(a))
    })
  }

  def joinI[I, B](implicit outer: IterateeT[X, E, F, A] =:= IterateeT[X, E, F, StepT[X, I, F, B]], M: Monad[F]): IterateeT[X, E, F, B] = {
    val ITP = IterateeT.IterateeTMonad[X, E, F]
    def check: StepT[X, I, F, B] => IterateeT[X, E, F, B] = _.fold(
      cont = k => k(eofInput) >>== {
        s => s.mapContOr(_ => sys.error("diverging iteratee"), check(s))
      }
      , done = (a, _) => ITP.point(a)
      , err = e => err(e)
    )

    outer(this) flatMap check
  }

  def %=[O](e: EnumerateeT[X, O, E, F, A])(implicit m: Monad[F]): IterateeT[X, O, F, A] = {
    (this >>== e).joinI[E, A]
  }

  /**
   * Feeds input elements to this iteratee until it is done, feeds the produced value to the 
   * inner iteratee.  Then this iteratee will start over, looping until the inner iteratee is done.
   */
  def sequenceI[B](implicit m: Monad[F]): EnumerateeT[X, E, A, F, B] = {
    def loop: EnumerateeT[X, E, A, F, B] = doneOr(checkEof)
    def checkEof: (Input[A] => IterateeT[X, A, F, B]) => IterateeT[X, E, F, StepT[X, A, F, B]] = k =>
      isEof[X, E, F] flatMap {
        eof =>
          if (eof) done(scont(k), eofInput)
          else step(k)
      }
    def step: (Input[A] => IterateeT[X, A, F, B]) => IterateeT[X, E, F, StepT[X, A, F, B]] = k =>
      this flatMap (a => k(elInput(a)) >>== loop)
    loop
  }

  def zip[B](other: IterateeT[X, E, F, B])(implicit F: Monad[F]): IterateeT[X, E, F, (A, B)] = {
    def step[Z](i: IterateeT[X, E, F, Z], in: Input[E]) =
      IterateeT.IterateeTMonadTrans[X, E].liftM(i.foldT[(Either[X, Option[(Z, Input[E])]], IterateeT[X, E, F, Z])](
        cont = k => F.point((Right(None), k(in)))
        , done = (a, x) => F.point((Right(Some((a, x))), done(a, x)))
        , err = e => F.point((Left(e), err(e)))
      ))
    def loop(x: IterateeT[X, E, F, A], y: IterateeT[X, E, F, B])(in: Input[E]): IterateeT[X, E, F, (A, B)] = in(
      el = _ =>
        step(x, in) flatMap {
          case (a, xx) =>
            step(y, in) flatMap {
              case (b, yy) =>
                (a, b) match {
                  case (Left(e), _)                                => err(e)
                  case (_, Left(e))                                => err(e)
                  case (Right(Some((a, e))), Right(Some((b, ee)))) => done((a, b), if (e.isEl) e else ee)
                  case _                                           => cont(loop(xx, yy))
                }
            }
        }
      , empty = cont(loop(x, y))
      , eof = (x >>== enumEofT(e => err(e))) flatMap (a => (y >>== enumEofT(e => err(e))) map (b => (a, b)))
    )
    cont(loop(this, other))
  }
}

object IterateeT extends IterateeTFunctions with IterateeTInstances {
  def apply[X, E, F[_], A](s: F[StepT[X, E, F, A]]): IterateeT[X, E, F, A] =
    iterateeT(s)
}

trait IterateeTInstances0 {
  implicit def IterateeTMonad[X, E, F[_]](implicit F0: Monad[F]): Monad[({type λ[α] = IterateeT[X, E, F, α]})#λ] = new IterateeTMonad[X, E, F] {
    implicit def F = F0
  }
  implicit def IterateeMonad[X, E]: Monad[({type λ[α] = Iteratee[X, E, α]})#λ] = IterateeTMonad[X, E, Id]
}

trait IterateeTInstances extends IterateeTInstances0 {
  implicit def IterateeTMonadTrans[X, E]: MonadTrans[({type λ[α[_], β] = IterateeT[X, E, α, β]})#λ] = new MonadTrans[({type λ[α[_], β] = IterateeT[X, E, α, β]})#λ] {
    def hoist[F[_]: Monad, G[_]](f: F ~> G) = new (({type f[x] = IterateeT[X, E, F, x]})#f ~> ({type f[x] = IterateeT[X, E, G, x]})#f) {
      def apply[A](fa: IterateeT[X, E, F, A]): IterateeT[X, E, G, A] = fa mapI f
    }

    def liftM[G[_] : Monad, A](ga: G[A]): IterateeT[X, E, G, A] =
      iterateeT(Monad[G].map(ga)((x: A) => StepT.sdone[X, E, G, A](x, emptyInput)))
  }

  implicit def IterateeTMonadIO[X, E, F[_]](implicit M0: MonadIO[F]): MonadIO[({type λ[α] = IterateeT[X, E, F, α]})#λ] =
    new IterateeTMonadIO[X, E, F] {
      implicit def F = M0
      implicit def G = M0
    }
}

trait IterateeTFunctions {
  def iterateeT[X, E, F[_], A](s: F[StepT[X, E, F, A]]): IterateeT[X, E, F, A] = new IterateeT[X, E, F, A] {
    val value = s
  }

  def cont[X, E, F[_] : Pointed, A](c: Input[E] => IterateeT[X, E, F, A]): IterateeT[X, E, F, A] =
    iterateeT(Pointed[F].point(StepT.scont(c)))

  def done[X, E, F[_] : Pointed, A](d: => A, r: => Input[E]): IterateeT[X, E, F, A] =
    iterateeT(Pointed[F].point(StepT.sdone(d, r)))

  def err[X, E, F[_] : Pointed, A](e: => X): IterateeT[X, E, F, A] =
    iterateeT(Pointed[F].point(StepT.serr(e)))

  /**
   * An iteratee that writes input to the output stream as it comes in.  Useful for debugging.
   */
  def putStrTo[X, E](os: java.io.OutputStream)(implicit s: Show[E]): IterateeT[X, E, IO, Unit] = {
    def write(e: E) = IO(os.write(s.shows(e).getBytes))
    foldM(())((_: Unit, e: E) => write(e))
  }

  /**
   * An iteratee that consumes all of the input into something that is Empty and Pointed. Useful for testing.
   */
  def consume[X, E, F[_]: Monad, A[_]: Empty : Pointed]: IterateeT[X, E, F, A[E]] = {
    import scalaz.syntax.plus._
    def step(e: Input[E]): IterateeT[X, E, F, A[E]] = 
      e.fold(empty = cont(step)
        , el = e => cont(step).map(a => implicitly[Pointed[A]].point(e) <+> a)
        , eof = done(implicitly[Empty[A]].empty, eofInput[E])
      )   

    cont(step)
  }

  /**An iteratee that consumes the head of the input **/
  def head[X, E, F[_] : Pointed]: IterateeT[X, E, F, Option[E]] = {
    def step(s: Input[E]): IterateeT[X, E, F, Option[E]] =
      s(empty = cont(step)
        , el = e => done(Some(e), emptyInput[E])
        , eof = done(None, eofInput[E])
      )
    cont(step)
  }

  def headDoneOr[X, E, F[_] : Monad, B](b: => B, f: E => IterateeT[X, E, F, B]): IterateeT[X, E, F, B] = {
    head[X, E, F] flatMap {
      case None => done(b, eofInput)
      case Some(a) => f(a)
    }
  }

  /**An iteratee that returns the first element of the input **/
  def peek[X, E, F[_] : Pointed]: IterateeT[X, E, F, Option[E]] = {
    def step(s: Input[E]): IterateeT[X, E, F, Option[E]]
    = s(el = e => done(Some(e), s),
      empty = cont(step),
      eof = done(None, eofInput[E]))
    cont(step)
  }

  def peekDoneOr[X, E, F[_] : Monad, B](b: => B, f: E => IterateeT[X, E, F, B]): IterateeT[X, E, F, B] = {
    peek[X, E, F] flatMap {
      case None => done(b, eofInput)
      case Some(a) => f(a)
    }
  }

  /**An iteratee that skips the first n elements of the input **/
  def drop[X, E, F[_] : Pointed](n: Int): IterateeT[X, E, F, Unit] = {
    def step(s: Input[E]): IterateeT[X, E, F, Unit] =
      s(el = _ => drop(n - 1),
        empty = cont(step),
        eof = done((), eofInput[E]))
    if (n == 0) done((), emptyInput[E])
    else cont(step)
  }

  /**
   * An iteratee that skips elements while the predicate evaluates to true.
   */
  def dropWhile[X, E, F[_] : Pointed](p: E => Boolean): IterateeT[X, E, F, Unit] = {
    def step(s: Input[E]): IterateeT[X, E, F, Unit] =
      s(el = e => if (p(e)) dropWhile(p) else done((), s),
        empty = cont(step),
        eof = done((), eofInput[E]))
    cont(step)
  }

  /**
   * An iteratee that skips elements until the predicate evaluates to true.
   */
  def dropUntil[X, E, F[_] : Pointed](p: E => Boolean): IterateeT[X, E, F, Unit] = dropWhile(!p(_))

  def fold[X, E, F[_] : Pointed, A](init: A)(f: (A, E) => A): IterateeT[X, E, F, A] = {
    def step(acc: A): Input[E] => IterateeT[X, E, F, A] = s =>
      s(el = e => cont(step(f(acc, e))),
        empty = cont(step(acc)),
        eof = done(acc, eofInput[E]))
    cont(step(init))
  }

  def foldM[X, E, F[_], A](init: A)(f: (A, E) => F[A])(implicit m: Monad[F]): IterateeT[X, E, F, A] = {
    def step(acc: A): Input[E] => IterateeT[X, E, F, A] = s =>
      s(el = e => IterateeT.IterateeTMonadTrans[X, E].liftM(f(acc, e)) flatMap (a => cont(step(a))),
        empty = cont(step(acc)),
        eof = done(acc, eofInput[E]))
    cont(step(init))
  }

  /**
   * An iteratee that counts and consumes the elements of the input
   */
  def length[X, E, F[_] : Pointed]: IterateeT[X, E, F, Int] = fold(0)((a, _) => a + 1)

  /**
   * An iteratee that checks if the input is EOF.
   */
  def isEof[X, E, F[_] : Pointed]: IterateeT[X, E, F, Boolean] = cont(in => done(in.isEof, in))

  private class NestedIterateeOps[X, E, F[_]: Monad] {
    import scalaz.syntax.Syntax.bind._
    import scalaz.syntax.Syntax.order._

    type IterateeM[A] = IterateeT[X, E, F, A]
    implicit val IterateeMM = IterateeT.IterateeTMonad[Unit, Int, IterateeM]
    
    def lift[A](iter: IterateeT[X, E, F, A]): IterateeT[X, E, IterateeM, A] = IterateeT.IterateeTMonadTrans[X, E].liftM[({type λ[α] = IterateeT[X, E, F, α]})#λ, A](iter)

    def end[A, EE](step: StepT[X, EE, F, A]): IterateeT[X, E, F, A] = {
      step.fold(
        cont = contf  => iterateeT(contf(eofInput).value >>= (s => end(s).value)), //todo: should throw a diverging iteratee error
        done = (a, _) => done(a, emptyInput),
        err  = x      => err(x)
      )
    }

    def mergeI[A](step: StepT[X, E, F, A])(implicit order: Order[E]): IterateeT[X, E, IterateeM, A] = {
      step.fold[IterateeT[X, E, IterateeM, A]](
        cont = contf => for {
          leftOpt <- head[X, E, IterateeM]
          rightOpt <- lift(peek[X, E, F])
          a <- {
          
            val nextStep: IterateeT[X, E, F, StepT[X, E, IterateeM, A]] = sys.error("todo") /*(leftOpt, rightOpt) match {
              case (Some(left), Some(right)) => 
                val (first, second) = if (right > left) (left, right) else (right, left)

                contf(elInput(first)) >>== { s: StepT[X, E, F, A] => 
                  s.fold[IterateeT[X, E, IterateeM, A]](
                    cont = contf => iterateeT[X, E, IterateeM, A](contf(elInput(second)) >>== (s => mergeI(s).value)),
                    done = (a, _) => done(a, emptyInput),
                    err  = x => err(x)
                  ).value
                }

              case (None, Some(right)) => contf(elInput(right)) >>== (s => mergeI(s).value)
              case (Some(left), None)  => contf(elInput(left))  >>== (s => mergeI(s).value)
              case _ => step.fold(
                cont = contf => iterateeT(
                  contf(eofInput).foldT(
                    cont = sys.error("diverging iteratee"),
                    done = (a, _)  => done[X, E, F, StepT[X, E, IterateeM, A]](sdone(a, emptyInput), emptyInput).value,
                    err  = x       => err[X, E, F, StepT[X, E, IterateeM, A]](x).value
                  )
                ),
                done = (a, _) => done[X, E, F, StepT[X, E, IterateeM, A]](sdone(a, emptyInput), emptyInput),
                err  = x      => err(x)
              )
            }
            */

            iterateeT[X, E, IterateeM, A](nextStep)
          }
        } yield a,
        done = (a, _) => done(a, emptyInput),
        err  = x => err(x)
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
                val joined = contf(elInput((left, right))) >>== (s => matchI(s).value)
                IterateeT[X, E, IterateeM, A](joined)

              case Some(_) => matchI(step)

              case None => lift(end(step))
            }
          } yield a
        },
        done = (a, _) => done(a, emptyInput),
        err  = x => err(x)
      )
    }

    def cogroupI[A](step: StepT[X, Either3[E, (E, E), E], F, A])(implicit order: Order[E]): IterateeT[X, E, IterateeM, A] = {
      import Either3._
      step.fold[IterateeT[X, E, IterateeM, A]](
        cont = contf => {
          for {
            leftOpt  <- peek[X, E, IterateeM]
            rightOpt <- lift(peek[X, E, F])
            a <- (leftOpt, rightOpt) match {
              case (left, Some(right)) if left.forall(right < _) => 
                for {
                  right <- lift(head[X, E, F])
                  a <- iterateeT[X, E, IterateeM, A](contf(elInput(right3(right.get))) >>== (s => cogroupI(s).value))
                } yield a

              case (Some(left), right) if right.forall(_ > left) => 
                for {
                  left <- head[X, E, IterateeM]
                  a <- iterateeT[X, E, IterateeM, A](contf(elInput(left3(left.get))) >>== (s => cogroupI(s).value))
                } yield a
          
              case (Some(left), Some(right)) => 
                for {
                  left <- head[X, E, IterateeM]
                  right <- lift(head[X, E, F])
                  a <- iterateeT[X, E, IterateeM, A](contf(elInput(middle3((left.get, right.get)))) >>== (s => cogroupI(s).value))
                } yield a

              case _ => lift(end(step))
            }
          } yield a
        },
        done = (a, _) => done(a, emptyInput),
        err  = x => err(x)
      )
    }
  }
  
  def matchI[X, E: Order, F[_]: Monad, A](step: StepT[X, (E, E), F, A]) = new NestedIterateeOps().matchI(step)

  def mergeI[X, E: Order, F[_]: Monad, A](step: StepT[X, E, F, A]) = new NestedIterateeOps().mergeI(step)
  
  def cogroupI[X, E: Order, F[_]: Monad, A](step: StepT[X, Either3[E, (E, E), E], F, A]) = new NestedIterateeOps().cogroupI(step)
}

//
// Type class implementation traits
//

private[scalaz] trait IterateeTMonad[X, E, F[_]] extends Monad[({type λ[α] = IterateeT[X, E, F, α]})#λ] {
  implicit def F: Monad[F]

  def point[A](a: => A) = StepT.sdone(a, emptyInput).pointI
  override def map[A, B](fa: IterateeT[X, E, F, A])(f: (A) => B): IterateeT[X, E, F, B] = fa map f
  def bind[A, B](fa: IterateeT[X, E, F, A])(f: A => IterateeT[X, E, F, B]): IterateeT[X, E, F, B] = fa flatMap f
}

private[scalaz] trait IterateeTMonadIO[X, E, F[_]] extends MonadIO[({type λ[α] = IterateeT[X, E, F, α]})#λ] with IterateeTMonad[X, E, F] {
  implicit def F: MonadIO[F]
  
  def liftIO[A](ioa: IO[A]) = MonadTrans[({type λ[α[_], β] = IterateeT[X, E, α, β]})#λ].liftM(F.liftIO(ioa))
}

package scalaz
package iteratee

import effect._
import Iteratee._

private[iteratee] class NestedIterateeT[X, E, F[_]](implicit FMonad: Monad[F]) {
  import scalaz.syntax.Syntax.bind._
  import scalaz.syntax.Syntax.order._

  type IterateeM[A] = IterateeT[X, E, F, A]
  implicit val IterateeMM = IterateeT.IterateeTMonad[Unit, Int, IterateeM]
  
  private def end[A, EE](step: StepT[X, EE, F, A]): IterateeT[X, E, F, A] = {
    step.fold(
      cont = contf  => iterateeT(contf(eofInput).value >>= (s => end(s).value)), //todo: should throw a diverging iteratee error
      done = (a, _) => done(a, emptyInput),
      err  = x      => err(x)
    )
  }

  def lift[A](iter: IterateeT[X, E, F, A]): IterateeT[X, E, IterateeM, A] = IterateeT.IterateeTMonadTrans[X, E].liftM[({type λ[α] = IterateeT[X, E, F, α]})#λ, A](iter)

  def mergeI[A](step: StepT[X, E, F, A])(implicit order: Order[E]): IterateeT[X, E, IterateeM, A] = {
    def estep(step: StepT[X, E, F, A]): StepT[X, Either3[E, (E, E), E], F, A] = step.fold(
      cont = contf => scont { in: Input[Either3[E, (E, E), E]] =>
        in.fold(
          el = _.fold(
            left   = a => contf(elInput(a)) >>== (s => estep(s).pointI),
            middle = b => contf(elInput(b._1)) >>== { s =>
              s.fold(
                cont = contf => contf(elInput(b._2)) >>== (s => estep(s).pointI),
                done = (a, _) => done(a, emptyInput[Either3[E, (E, E), E]]),
                err  = x => err(x)
              )
            },
            right  = c => contf(elInput(c)) >>== (s => estep(s).pointI)
          ),
          empty = contf(emptyInput[E]) >>== (s => estep(s).pointI), 
          eof =   contf(eofInput[E]) >>== (s => estep(s).pointI)
        )
      },
      done = (a, _) => sdone(a, emptyInput[Either3[E, (E, E), E]]),
      err  = x => serr(x)
    )

    cogroupI(estep(step))
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

  def cross1[A](step: StepT[X, (E, E), F, A]): IterateeT[X, E, IterateeM, StepT[X, (E, E), F, A]] = {
    step.fold[IterateeT[X, E, IterateeM, StepT[X, (E, E), F, A]]](
      cont = contf =>
        for {
          leftOpt  <- peek[X, E, IterateeM]
          rightOpt <- lift(head[X, E, F])
          sa       <- (leftOpt, rightOpt) match {
                        case (Some(left), Some(right)) => iterateeT[X, E, IterateeM, StepT[X, (E, E), F, A]](contf(elInput((left, right))) >>== (s => cross1(s).value))
                        case _                         => done[X, E, IterateeM, StepT[X, (E, E), F, A]](step, eofInput)
                      }
        } yield sa,
      done = (_, _) => done(step, emptyInput),
      err = x => err(x)
    )
  }

}

private[iteratee] class NestedEnumeratorT[X, E] {
  // frequently used type lambdas
  private[iteratee] class FG[F[_[_], _], G[_]] {
    type FGA[A] = F[G, A]
    type IterateeM[A] = IterateeT[X, E, FGA, A]
  }

  def crossE[A, G[_]: Monad](e1: EnumeratorP[X, E, G], e2: EnumeratorP[X, E, G]): EnumeratorP[X, (E, E), G] = new EnumeratorP[X, (E, E), G] {
    def apply[F[_[_], _], A](implicit t: MonadTrans[F], MF: Monad[FG[F, G]#FGA]): EnumeratorT[X, (E, E), FG[F, G]#FGA, A] = {
      val e1t = e1.apply[F, A]
      implicit val iterateeTM = IterateeT.IterateeTMonad[X, E, FG[F, G]#FGA]

      def outerLoop(step: StepT[X, (E, E), FG[F, G]#FGA, A]) : IterateeT[X, (E, E), FG[F, G]#FGA, A] = {
        val iteratee = for {
          leap <- cross1[X, E, FG[F, G]#FGA, A](step)
          _    <- drop[X, E, FG[F, G]#FGA](1) 
          eof  <- isEof[X, E, FG[F, G]#FGA]
          sa   <- if (eof) done[X, (E, E), FG[F, G]#FGA, StepT[X, (E, E), FG[F, G]#FGA, A]](step, eofInput) else outerLoop(leap)
        } yield sa

        (iteratee >>== e1t).run(x => err(x).value)
      }

      outerLoop
    }
  }
}

private abstract class EnumeratorP[X, E, G[_]: Monad] {
  def apply[F[_[_], _], A](implicit t: MonadTrans[F]): EnumeratorT[X, E, ({type λ[α] = F[G, α]})#λ, A]
}

// vim: set ts=4 sw=4 et:

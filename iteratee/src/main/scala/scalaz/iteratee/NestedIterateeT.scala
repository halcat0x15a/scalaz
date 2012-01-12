package scalaz
package iteratee

import effect._
import Iteratee._

private[iteratee] class NestedIterateeT[X, E, F[_]](implicit FMonad: Monad[F]) {
  import scalaz.syntax.Syntax.bind._
  import scalaz.syntax.Syntax.order._

  type IterateeM[A] = IterateeT[X, E, F, A]

  type MergeStepT[A] = StepT[X, E, F, A]
  type MatchStepT[A] = StepT[X, (E, E), F, A]
  type CogroupStepT[A] = StepT[X, Either3[E, (E, E), E], F, A]

  private def lift[A](iter: IterateeT[X, E, F, A]): IterateeT[X, E, IterateeM, A] = 
    IterateeT.IterateeTMonadTrans[X, E].liftM[({type λ[α] = IterateeT[X, E, F, α]})#λ, A](iter)

  private def endStep[EE, A](sa: CogroupStepT[A]): StepT[X, EE, F, A] = {
    sa.fold(
      cont = sys.error("diverging iteratee"),
      done = (a, r) => sdone[X, EE, F, A](a, if (r.isEof) eofInput[EE] else emptyInput[EE]),
      err  = x      => serr[X, EE, F, A](x)
    )
  }

  def mergeI[A](step: MergeStepT[A])(implicit order: Order[E]): IterateeT[X, E, IterateeM, MergeStepT[A]] = {
    def estep(step: MergeStepT[A]): CogroupStepT[A]  = step.fold(
      cont = contf => scont { in: Input[Either3[E, (E, E), E]] =>
        in.fold(
          el = _.fold(
            left   = a => contf(elInput(a)) >>== (s => estep(s).pointI),
            middle = b => contf(elInput(b._1)) >>== { s =>
              s.fold(
                cont = contf  => contf(elInput(b._2)) >>== (s => estep(s).pointI),
                done = (a, r) => done(a, if (r.isEof) eofInput else emptyInput),
                err  = x      => err(x)
              )
            },
            right  = c => contf(elInput(c)) >>== (s => estep(s).pointI)
          ),
          empty = contf(emptyInput[E]) >>== (s => estep(s).pointI), 
          eof =   contf(eofInput[E]) >>== (s => estep(s).pointI)
        )
      },
      done = (a, r) => sdone(a, if (r.isEof) eofInput else emptyInput),
      err  = x => serr(x)
    )

    cogroupI(estep(step)).map(endStep[E, A])
  }

  def mergeDistinctI[A](step: MergeStepT[A])(implicit order: Order[E]): IterateeT[X, E, IterateeM, MergeStepT[A]] = {
    def estep(step: MergeStepT[A]): CogroupStepT[A]  = step.fold(
      cont = contf => scont { in: Input[Either3[E, (E, E), E]] =>
        val nextInput = in.map(_.fold(identity[E], _._1, identity[E]))

        contf(nextInput) >>== (s => estep(s).pointI)
      },
      done = (a, r) => sdone(a, if (r.isEof) eofInput else emptyInput),
      err  = x => serr(x)
    )

    cogroupI(estep(step)).map(endStep[E, A])
  }

  def matchI[A](step: MatchStepT[A])(implicit order: Order[E]): IterateeT[X, E, IterateeM, MatchStepT[A]] = {
    def estep(step: MatchStepT[A]): CogroupStepT[A] = step.fold(
      cont = contf => scont { in: Input[Either3[E, (E, E), E]] =>
        val nextInput = in.flatMap(_.middleOr(emptyInput[(E, E)]) { elInput(_) })

        contf(nextInput) >>== (s => estep(s).pointI)
      },
      done = (a, r) => sdone(a, if (r.isEof) eofInput else emptyInput),
      err  = x => serr(x)
    )
    
    cogroupI(estep(step)).map(endStep[(E, E), A])
  }

  def cogroupI[A](step: CogroupStepT[A])(implicit order: Order[E]): IterateeT[X, E, IterateeM, CogroupStepT[A]] = {
    import Either3._
    step.fold[IterateeT[X, E, IterateeM, CogroupStepT[A]]](
      cont = contf => {
        for {
          leftOpt  <- peek[X, E, IterateeM]
          rightOpt <- lift(peek[X, E, F])
          a <- (leftOpt, rightOpt) match {
            case (left, Some(right)) if left.forall(right < _) => 
              for {
                right <- lift(head[X, E, F])
                a <- iterateeT[X, E, IterateeM, CogroupStepT[A]](contf(elInput(right3(right.get))) >>== (s => cogroupI(s).value))
              } yield a

            case (Some(left), right) if right.forall(_ > left) => 
              for {
                left <- head[X, E, IterateeM]
                a <- iterateeT[X, E, IterateeM, CogroupStepT[A]](contf(elInput(left3(left.get))) >>== (s => cogroupI(s).value))
              } yield a
        
            case (Some(left), Some(right)) => 
              for {
                left <- head[X, E, IterateeM]
                right <- lift(head[X, E, F])
                a <- iterateeT[X, E, IterateeM, CogroupStepT[A]](contf(elInput(middle3((left.get, right.get)))) >>== (s => cogroupI(s).value))
              } yield a

            case _ => done[X, E, IterateeM, CogroupStepT[A]](step, eofInput)
          }
        } yield a
      },
      done = (a, r) => done[X, E, IterateeM, CogroupStepT[A]](sdone(a, if (r.isEof) eofInput else emptyInput), if (r.isEof) eofInput else emptyInput), 
      err  = x => err(x)
    )
  }
}

// vim: set ts=4 sw=4 et:

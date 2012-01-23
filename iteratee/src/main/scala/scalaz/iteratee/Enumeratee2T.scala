package scalaz
package iteratee

import effect._
import Iteratee._

trait Enumeratee2TFunctions {
  import scalaz.syntax.Syntax.bind._
  import scalaz.syntax.Syntax.order._

  def cogroupI[X, E: Order, F[_]: Monad, A]: Enumeratee2T[X, E, Either3[E, (E, E), E], F, A] = {
    type IterateeM[A] = IterateeT[X, E, F, A]
    type StepM[A] = StepT[X, Either3[E, (E, E), E], F, A]

    @inline def lift[A](iter: IterateeT[X, E, F, A]): IterateeT[X, E, IterateeM, A] = 
      IterateeT.IterateeTMonadTrans[X, E].liftM[({type λ[α] = IterateeT[X, E, F, α]})#λ, A](iter)

    def loop[A](step: StepM[A]): IterateeT[X, E, IterateeM, StepM[A]] = {
      import Either3._
      step.fold[IterateeT[X, E, IterateeM, StepM[A]]](
        cont = contf => {
          for {
            leftOpt  <- peek[X, E, IterateeM]
            rightOpt <- lift(peek[X, E, F])
            a <- (leftOpt, rightOpt) match {
              case (left, Some(right)) if left.forall(right < _) => 
                for {
                  right <- lift(head[X, E, F])
                  a <- iterateeT[X, E, IterateeM, StepM[A]](contf(elInput(right3(right.get))) >>== (s => loop(s).value))
                } yield a

              case (Some(left), right) if right.forall(_ > left) => 
                for {
                  left <- head[X, E, IterateeM]
                  a <- iterateeT[X, E, IterateeM, StepM[A]](contf(elInput(left3(left.get))) >>== (s => loop(s).value))
                } yield a
          
              case (Some(left), Some(right)) => 
                for {
                  left <- head[X, E, IterateeM]
                  right <- lift(head[X, E, F])
                  a <- iterateeT[X, E, IterateeM, StepM[A]](contf(elInput(middle3((left.get, right.get)))) >>== (s => loop(s).value))
                } yield a

              case _ => done[X, E, IterateeM, StepM[A]](step, eofInput)
            }
          } yield a
        },
        done = (a, r) => done[X, E, IterateeM, StepM[A]](sdone(a, if (r.isEof) eofInput else emptyInput), if (r.isEof) eofInput else emptyInput), 
        err  = x => err(x)
      )
    }
    
    loop _
  }

  def matchI[X, E: Order, F[_]: Monad, A]: Enumeratee2T[X, E, (E, E), F, A] = {
    def cstep(step: StepT[X, (E, E), F, A]): StepT[X, Either3[E, (E, E), E], F, A] = step.fold(
      cont = contf => scont { in: Input[Either3[E, (E, E), E]] =>
        val nextInput = in.flatMap(_.middleOr(emptyInput[(E, E)]) { elInput(_) })

        contf(nextInput) >>== (s => cstep(s).pointI)
      },
      done = (a, r) => sdone(a, if (r.isEof) eofInput else emptyInput),
      err  = x => serr(x)
    )
      
    (step: StepT[X, (E, E), F, A]) => cogroupI.apply(cstep(step)).map(endStep[X, E, (E, E), F, A])
  }

  def mergeI[X, E: Order, F[_]: Monad, A]: Enumeratee2T[X, E, E, F, A] = {
    def cstep(step: StepT[X, E, F, A]): StepT[X, Either3[E, (E, E), E], F, A] = step.fold(
      cont = contf => scont { in: Input[Either3[E, (E, E), E]] =>
        in.fold(
          el = _.fold(
            left   = a => contf(elInput(a)) >>== (s => cstep(s).pointI),
            middle = b => contf(elInput(b._1)) >>== { s =>
              s.fold(
                cont = contf  => contf(elInput(b._2)) >>== (s => cstep(s).pointI),
                done = (a, r) => done(a, if (r.isEof) eofInput else emptyInput),
                err  = x      => err(x)
              )
            },
            right  = c => contf(elInput(c)) >>== (s => cstep(s).pointI)
          ),
          empty = contf(emptyInput[E]) >>== (s => cstep(s).pointI), 
          eof =   contf(eofInput[E]) >>== (s => cstep(s).pointI)
        )
      },
      done = (a, r) => sdone(a, if (r.isEof) eofInput else emptyInput),
      err  = x => serr(x)
    )

    (step: StepT[X, E, F, A]) => cogroupI.apply(cstep(step)).map(endStep[X, E, E, F, A])
  }
  
  def mergeDistinctI[X, E: Order, F[_]: Monad, A]: Enumeratee2T[X, E, E, F, A] = {
    def cstep(step: StepT[X, E, F, A]): StepT[X, Either3[E, (E, E), E], F, A]  = step.fold(
      cont = contf => scont { in: Input[Either3[E, (E, E), E]] =>
        val nextInput = in.map(_.fold(identity[E], _._1, identity[E]))

        contf(nextInput) >>== (s => cstep(s).pointI)
      },
      done = (a, r) => sdone(a, if (r.isEof) eofInput else emptyInput),
      err  = x => serr(x)
    )

    (step: StepT[X, E, F, A]) => cogroupI.apply(cstep(step)).map(endStep[X, E, E, F, A])
  }

  private def endStep[X, E, EE, F[_], A](sa: StepT[X, Either3[E, (E, E), E], F, A]): StepT[X, EE, F, A] = {
    sa.fold(
      cont = sys.error("diverging iteratee"),
      done = (a, r) => sdone[X, EE, F, A](a, if (r.isEof) eofInput[EE] else emptyInput[EE]),
      err  = x      => serr[X, EE, F, A](x)
    )
  }
}

// vim: set ts=4 sw=4 et:

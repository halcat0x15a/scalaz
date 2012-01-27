package scalaz
package iteratee

import effect._
import Iteratee._

trait Enumeratee2T[X, O, I, F[_]] {
  type IterateeM[α] = IterateeT[X, O, F, α]
  type StepM[α] = StepT[X, I, F, α]

  def apply[A]: StepM[A] => IterateeT[X, O, IterateeM, StepM[A]]
}

trait Enumeratee2TFunctions {
  import scalaz.syntax.Syntax.bind._
  import scalaz.syntax.Syntax.order._

  @inline private def lift[X, E, F[_]: Monad, A](iter: IterateeT[X, E, F, A]): IterateeT[X, E, ({type λ[α] = IterateeT[X, E, F, α] })#λ, A] = 
    IterateeT.IterateeTMonadTrans[X, E].liftM[({type λ[α] = IterateeT[X, E, F, α]})#λ, A](iter)

  def cogroupI[X, E: Order, F[_]: Monad]: Enumeratee2T[X, E, Either3[E, (E, E), E], F] = 
    new Enumeratee2T[X, E, Either3[E, (E, E), E], F] {
      def apply[A] = (step: StepM[A]) => {
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
                    a <- iterateeT[X, E, IterateeM, StepM[A]](contf(elInput(right3(right.get))) >>== (s => apply(s).value))
                  } yield a

                case (Some(left), right) if right.forall(_ > left) => 
                  for {
                    left <- head[X, E, IterateeM]
                    a <- iterateeT[X, E, IterateeM, StepM[A]](contf(elInput(left3(left.get))) >>== (s => apply(s).value))
                  } yield a
            
                case (Some(left), Some(right)) => 
                  for {
                    left <- head[X, E, IterateeM]
                    right <- lift(head[X, E, F])
                    a <- iterateeT[X, E, IterateeM, StepM[A]](contf(elInput(middle3((left.get, right.get)))) >>== (s => apply(s).value))
                  } yield a

                case _ => done[X, E, IterateeM, StepM[A]](step, eofInput)
              }
            } yield a
          },
          done = (a, r) => done[X, E, IterateeM, StepM[A]](sdone(a, if (r.isEof) eofInput else emptyInput), if (r.isEof) eofInput else emptyInput), 
          err  = x => err(x)
        )
      }
    }

  def matchI[X, E: Order, F[_]: Monad]: Enumeratee2T[X, E, (E, E), F] = 
    new Enumeratee2T[X, E, (E, E), F] {
      def apply[A] = {
        def cstep(step: StepT[X, (E, E), F, A]): StepT[X, Either3[E, (E, E), E], F, StepT[X, (E, E), F, A]] = step.fold(
          cont = contf => scont { in: Input[Either3[E, (E, E), E]] =>
            val nextInput = in.flatMap(_.middleOr(emptyInput[(E, E)]) { elInput(_) })

            contf(nextInput) >>== (s => cstep(s).pointI)
          },
          done = (a, r) => sdone(sdone(a, if (r.isEof) eofInput else emptyInput), if (r.isEof) eofInput else emptyInput),
          err  = x => serr(x)
        )
      
        (step: StepT[X, (E, E), F, A]) => cogroupI.apply(cstep(step)) flatMap { endStep[X, E, (E, E), F, A] }
      }
    }

  def mergeI[X, E: Order, F[_]: Monad]: Enumeratee2T[X, E, E, F] = 
    new Enumeratee2T[X, E, E, F] {
      def apply[A] = {
        def cstep(step: StepT[X, E, F, A]): StepT[X, Either3[E, (E, E), E], F, StepT[X, E, F, A]] = step.fold(
          cont = contf => scont { in: Input[Either3[E, (E, E), E]] =>
            in.fold(
              el = _.fold(
                left   = a => contf(elInput(a)) >>== (s => cstep(s).pointI),
                middle = b => contf(elInput(b._1)) >>== { s =>
                  s.fold(
                    cont = contf  => contf(elInput(b._2)) >>== (s => cstep(s).pointI),
                    done = (a, r) => done(sdone(a, if (r.isEof) eofInput else emptyInput), if (r.isEof) eofInput else emptyInput),
                    err  = x      => err(x)
                  )
                },
                right  = c => contf(elInput(c)) >>== (s => cstep(s).pointI)
              ),
              empty = contf(emptyInput[E]) >>== (s => cstep(s).pointI), 
              eof =   contf(eofInput[E]) >>== (s => cstep(s).pointI)
            )
          },
          done = (a, r) => sdone(sdone(a, if (r.isEof) eofInput else emptyInput), if (r.isEof) eofInput else emptyInput),
          err  = x => serr(x)
        )

        (step: StepT[X, E, F, A]) => cogroupI.apply(cstep(step)) flatMap { endStep[X, E, E, F, A] }
      }
    }
  
  def mergeDistinctI[X, E: Order, F[_]: Monad]: Enumeratee2T[X, E, E, F] = 
    new Enumeratee2T[X, E, E, F] {
      def apply[A] = {
        def cstep(step: StepT[X, E, F, A]): StepT[X, Either3[E, (E, E), E], F, StepT[X, E, F, A]]  = step.fold(
          cont = contf => scont { in: Input[Either3[E, (E, E), E]] =>
            val nextInput = in.map(_.fold(identity[E], _._1, identity[E]))

            contf(nextInput) >>== (s => cstep(s).pointI)
          },
          done = (a, r) => sdone(sdone(a, if (r.isEof) eofInput else emptyInput), if (r.isEof) eofInput else emptyInput),
          err  = x => serr(x)
        )

        (step: StepT[X, E, F, A]) => cogroupI.apply(cstep(step)) flatMap { endStep[X, E, E, F, A] }
      }
    }

  private def endStep[X, E, EE, F[_]: Monad, A](sa: StepT[X, Either3[E, (E, E), E], F, StepT[X, EE, F, A]]) = {
    IterateeT.IterateeTMonadTransT[X, E, ({ type λ[β[_], α] = IterateeT[X, E, β, α] })#λ].liftM(sa.pointI.run(x => err[X, EE, F, A](x).value))
  }
}

object Enumeratee2T extends Enumeratee2TFunctions

// vim: set ts=4 sw=4 et:

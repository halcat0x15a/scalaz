package scalaz
package iteratee

import Iteratee._

private[iteratee] class NestedEnumeratorT[X, E] {
  import scalaz.syntax.Syntax.bind._
  import scalaz.syntax.Syntax.order._

  // frequently used type lambdas
  def lift[A, EE, F[_]: Monad](iter: IterateeT[X, EE, F, A]): IterateeT[X, EE, ({type λ[α] = IterateeT[X, EE, F, α]})#λ, A] = IterateeT.IterateeTMonadTrans[X, EE].liftM[({type λ[α] = IterateeT[X, EE, F, α]})#λ, A](iter)

  private[iteratee] class FG[F[_[_], _], G[_]] {
    type FGA[A] = F[G, A]
    type IterateeM[A] = IterateeT[X, E, FGA, A]
    type StepM[A] = StepT[X, (E, E), FGA, A]
  }

  trait Value[A] {
    def value: A
  }

  def crossE[G[_]: Monad](e1: EnumeratorP[X, E, G], e2: EnumeratorP[X, E, G]): EnumeratorP[X, (E, E), G] = new EnumeratorP[X, (E, E), G] {
    def apply[F[_[_], _], A](implicit t: MonadTrans[F]): EnumeratorT[X, (E, E), FG[F, G]#FGA, A] = new Value[EnumeratorT[X, (E, E), FG[F, G]#FGA, A]] {
      import t.apply
      type FGA[α] = FG[F, G]#FGA[α]
      type IterateeM[α] = FG[F, G]#IterateeM[α]
      type StepM = FG[F, G]#StepM[A]

      implicit val IterateeTM  = IterateeT.IterateeTMonad[X, E, FGA]
      implicit val IterateeTMT = IterateeT.IterateeTMonadTransT[X, E, F]

      def value: StepM => IterateeT[X, (E, E), FGA, A] = (step: StepM) => {
        val e1t = e1.apply[F, StepM]
        val e2t = e2.apply[F, StepM]

        def outerLoop(step: StepM): IterateeT[X, E, FGA, StepM] =
          for {
            outerOpt   <- head[X, E, FGA]
            sa         <- outerOpt match {
                            case Some(e) => 
                              val pairingIteratee = EnumerateeT.map[X, E, (E, E), FGA, A]((a: E) => (e, a)).apply(step)
                              val nextStep = (pairingIteratee >>== e2t).run(x => err[X, (E, E), FGA, A](x).value)
                              iterateeT[X, (E, E), FGA, A](nextStep) >>== outerLoop

                            case None    => 
                              done[X, E, FGA, StepM](step, eofInput) 
                          }
          } yield sa

        iterateeT[X, (E, E), FGA, A] {
          (outerLoop(step) >>== e1t).run(x => err[X, (E, E), FGA, A](x).value)
        }
      }
    }.value
  }

  /*
  def cogroupE[G[_]](e1 : EnumeratorP[X, E, G], e2 : EnumeratorP[X, E, G])(implicit gm : Monad[G], order : Order[E]) = new EnumeratorP[X, Either3[E, (E, E), E], G] {
    def apply[F[_[_], _], A](implicit t: MonadTrans[F]): EnumeratorT[X, Either3[E, (E, E), E], FG[F, G]#FGA, A] = new Value[EnumeratorT[X, Either3[E, (E, E), E], FG[F, G]#FGA, A]] {
      import Either3._
      import t.apply
      type FGA[α] = FG[F, G]#FGA[α]
      type IterateeM[α] = FG[F, G]#IterateeM[α]
      type StepM = StepT[X, Either3[E, (E, E), E], FGA, A]

      implicit val IterateeTM  = IterateeT.IterateeTMonad[X, E, FGA]
      implicit val IterateeTMT = IterateeT.IterateeTMonadTransT[X, E, F]

      // Shorten things a bit
      type IterateeE3 = IterateeT[X, Either3[E, (E, E), E], FGA, A]      
      type IterateeOM = IterateeT[X, E, ({ type λ[α] = IterateeT[X, E, FGA, α] })#λ, StepM]

      def value: StepM => IterateeE3 = (step : StepM) => {
        val e1t = e1.apply[({ type λ[β[_], α] = IterateeT[X, E, ({ type L[γ] = F[β, γ] })#L, α]})#λ, StepM]
        val e2t = e2.apply[F, StepM]

        def loop(loopStep : StepM) : IterateeOM = {
          loopStep.fold[IterateeOM](
            cont = contf => {
              for {
                  leftOpt  <- peek[X, E, IterateeM]
                  rightOpt <- lift(peek[X, E, FGA])
                  a <- (leftOpt, rightOpt) match {
                    case (left, Some(right)) if left.forall(right < _) => 
                      for {
                        right <- lift(head[X, E, FGA])
                        a <- iterateeT[X, E, ({ type λ[α] = IterateeT[X, E, FGA, α] })#λ, StepM](null) // iterateeT[X, E, ({ type λ[α] = IterateeT[X, E, FGA, α] })#λ, StepM](contf(elInput(right3(right.get))) >>== loop)
                      } yield a

                    case (Some(left), right) if right.forall(_ > left) => 
                      for {
                        left <- head[X, E, IterateeM]
                        a <- iterateeT[X, E, IterateeM, A](contf(elInput(left3(left.get))) >>== (s => loop(s).value))
                      } yield a
                  
                    case (Some(left), Some(right)) => 
                      for {
                        left <- head[X, E, IterateeM]
                        right <- lift(head[X, E, FGA])
                        a <- iterateeT[X, E, IterateeM, A](contf(elInput(middle3((left.get, right.get)))) >>== (s => loop(s).value))
                      } yield a

                    case _ => lift(end(loopStep))
                  }
                } yield a
                        
            },
            done = (a, _) => done(a, emptyInput),
            err  = x => err(x)
          )
        }

        loop(step)
      }
    }.value
  }
  */

  /*
  def cogroupE[G[_]](e1 : EnumeratorP[X, E, G], e2 : EnumeratorP[X, E, G])(implicit gm : Monad[G], order : Order[E]) = new EnumeratorP[X, Either3[E, (E, E), E], G] {
    def apply[F[_[_], _], A](implicit t: MonadTrans[F]): EnumeratorT[X, Either3[E, (E, E), E], FG[F, G]#FGA, A] = new Value[EnumeratorT[X, Either3[E, (E, E), E], FG[F, G]#FGA, A]] {

      import Either3._
      import t.apply
      type FGA[α] = FG[F, G]#FGA[α]
      type IterateeM[α] = FG[F, G]#IterateeM[α]
      type StepM = StepT[X, Either3[E, (E, E), E], FGA, A]
      type IterateeE3 = IterateeT[X, Either3[E, (E, E), E], FGA, A]      

      val e1t = e1.apply[?, ?]
      //val e1t : String = e1.apply[({ type λ[β[_], α] = IterateeT[X, E, ({ type L[γ] = F[β, γ] })#L, α]})#λ, StepM]
      val e2t = e2.apply[F, A]

      val nested = new NestedIterateeT[X, E, G]

      val value : StepM => IterateeE3 = (step : StepM) => {
        (cogroupI(step) >>== e1t) // Um, now what?
      }
    }.value
  }
  */

  def cogroupE[G[_]](eLeft : EnumeratorP[X, E, G], eRight : EnumeratorP[X, E, G])(implicit gm : Monad[G], order : Order[E]) = new EnumeratorP[X, Either3[E, (E, E), E], G] {
    def apply[F[_[_], _], A](implicit t: MonadTrans[F]): EnumeratorT[X, Either3[E, (E, E), E], FG[F, G]#FGA, A] = new Value[EnumeratorT[X, Either3[E, (E, E), E], FG[F, G]#FGA, A]] {
      import Either3._
      import t.apply
      type FGA[α] = FG[F, G]#FGA[α]
      type IterateeM[α] = FG[F, G]#IterateeM[α]
      type StepM = StepT[X, Either3[E, (E, E), E], FGA, A]
      type IterateeE3 = IterateeT[X, Either3[E, (E, E), E], FGA, A]      

      val eLt = eLeft.apply[F, A]
      val eRt = eRight.apply[F, A]
      
      val value : StepM => IterateeE3 = (step : StepM) => {
        def loop(s : StepM) : IterateeE3 = {
          s.fold(cont = contf => {
                   eLt(scont({ (inL : Input[E]) =>
                     inL.fold(el = elLeft => {
                              null
                            },
                            empty = null,
                            eof = finishRight(s))
                   }))
                 },
                 done = (a, i) => done(a, i),
                 err = x => err(x))
        }

        def loopLeft(s : StepM, right : E) : IterateeE3 = {
        }

        def loopRight(s : StepM, left : E) : IterateeE3 = {
          s.fold(cont = contf => {
            eRt(scont({ (inR : Input[E]) =>
              inR.fold(el = elRight => {
                       if (left > elRight) {
                         contf(elInput(right3(elRight))) >>== (s => loopRight(s, left))
                       } else if (left < el2) {
                         contf(elInput(left3(left))) >>== (s => loopLeft(s, elRight))
                       } else {
                         contf(elInput(middle3((left, elRight)))) >>== (s => loop(s))
                       }
                     },
                     empty = loopRight(s, left),
                     eof = contf(elInput(left3(left))) >>== (s => finishLeft(s)))
                     }))
                 },
                 done = (a, i) => done(a, i),
                 err = x => err(x))
        }

        def finishLeft(s : StepM) : IterateeE3 = {
          s.fold(cont = contf => {
                   eLt(scont({ (inL : Input[E]) =>
                     inL.fold(el = elLeft => { contf(elInput(elLeft)) >>== (s => finishLeft(s)) },
                              empty = finishLeft(s),
                              eof = contf(eofInput))
                 }))},
                 done = (a, i) => done(a, i),
                 err = x => err(x))
        }

        def finishRight(s : StepM) : IterateeE3 = {

        }

      }
    }.value
  }
}


// vim: set ts=4 sw=4 et:

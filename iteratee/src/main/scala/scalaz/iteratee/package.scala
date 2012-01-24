package scalaz

package object iteratee {

  type Step[X, E, A] =
  StepT[X, E, Id, A]

  type Iteratee[X, E, A] =
  IterateeT[X, E, Id, A]

  object Iteratee
    extends IterateeFunctions
    with IterateeTFunctions
    with EnumeratorTFunctions
    with EnumeratorPFunctions
    with EnumerateeTFunctions
    with StepTFunctions
    with InputFunctions {

    def apply[X, E, A](s: Step[X, E, A]): Iteratee[X, E, A] = iteratee(s)
  }

  type Enumerator[X, E] =
  EnumeratorT[X, E, Id]

  type Enumeratee[X, O, I] = 
  EnumerateeT[X, O, I, Id]

  type Enumeratee2T[X, O, I, F[_], A] = 
  StepT[X, I, F, A] => IterateeT[X, O, ({type λ[α] = IterateeT[X, O, F, α]})#λ, StepT[X, I, F, A]]

  object Enumeratee2T extends Enumeratee2TFunctions

  type Iter[E, F[_], A] =
  IterateeT[Unit, E, F, A]

  type >@>[E, A] =
  Iteratee[Unit, E, A]

  type Enum[E] =
  Enumerator[Unit, E]
}

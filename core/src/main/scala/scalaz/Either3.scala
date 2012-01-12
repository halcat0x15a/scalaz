package scalaz

import scalaz.syntax.equal._
import scalaz.syntax.show._

sealed trait Either3[A, B, C] {
  def fold[Z](left: A => Z, middle: B => Z, right: C => Z): Z
  def eitherLeft:  Either[Either[A, B], C] = fold( 
    left   = a => Left(Left(a)),
    middle = b => Left(Right(b)),
    right  = c => Right(c)
  )

  def eitherRight: Either[A, Either[B, C]] = fold( 
    left   = a => Left(a),
    middle = b => Right(Left(b)),
    right  = c => Right(Right(c))
  )

  def leftOr[Z](z: => Z)(f: A => Z) = fold(f, _ => z, _ => z)
  def middleOr[Z](z: => Z)(f: B => Z) = fold(_ => z, f, _ => z)
  def rightOr[Z](z: => Z)(f: C => Z) = fold(_ => z, _ => z, f)
}

object Either3 {
  def left3[A, B, C](a: A) = new Either3[A, B, C] {
    def fold[Z](left: A => Z, middle: B => Z, right: C => Z): Z = left(a)
  }

  def middle3[A, B, C](b: B) = new Either3[A, B, C] {
    def fold[Z](left: A => Z, middle: B => Z, right: C => Z): Z = middle(b)
  }

  def right3[A, B, C](c: C) = new Either3[A, B, C] {
    def fold[Z](left: A => Z, middle: B => Z, right: C => Z): Z = right(c)
  }

  implicit def equal[A: Equal, B: Equal, C: Equal]: Equal[Either3[A, B, C]] = new Equal[Either3[A, B, C]] {
    def equal(a1: Either3[A, B, C], a2: Either3[A, B, C]) = {
      a1.fold(
        a => a2.fold(
          aa => a === aa,
          bb => false,
          cc => false
        ),
        b => a2.fold(
          aa => false,
          bb => b === bb,
          cc => false
        ),
        c => a2.fold(
          aa => false,
          bb => false,
          cc => c === cc
        )
      )
    }
  }

  implicit def show[A: Show, B: Show, C: Show]: Show[Either3[A, B, C]] = new Show[Either3[A, B, C]] {
    def show(v: Either3[A, B, C]) = shows(v).toCharArray.toList
    override def shows(v: Either3[A, B, C]) = {
      v.fold(
        a => "Left3(" + a.shows + ")",
        b => "Middle3(" + b.shows + ")",
        c => "Right3(" + c.shows + ")"
      )
    }
  }
}
    

// vim: set ts=4 sw=4 et:

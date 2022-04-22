package homework3

sealed trait Validated[+E, +A]:
  def isValid: Boolean = ???

  def getOrElse[B >: A](default: => B): B = ???

  def orElse[F >: E, B >: A](default: => Validated[F, B]): Validated[F, B] = ???

  def zip[EE >: E, B](vb: Validated[EE, B]): Validated[EE, (A, B)] = ???

  def map[B](f: A => B): Validated[E, B] = ???

  def zipMap[EE >: E, B, R](vb: Validated[EE, B])(f: (A, B) => R): Validated[EE, R] = ???

  def flatMap[EE >: E, B](f: A => Validated[EE, B]): Validated[EE, B] = ???

  def fold[B](fInvalid: Chain[E] => B, f: A => B): B = ???

  def foreach(f: A => Unit): Unit = fold(_ => (), f)

case class Valid[+A](value: A) extends Validated[Nothing, A]
case class Invalid[+E](errors: Chain[E]) extends Validated[E, Nothing]

object Invalid:
  def apply[E](error: E): Invalid[E] = Invalid(Chain(error))

object Validated:
  extension [EE, A, B](
    tuple: (
      Validated[EE, A],
      Validated[EE, B]
    )
  )
    def zipN: Validated[EE, (A, B)] = ???
    def mapN[R](f: (A, B) => R): Validated[EE, R] = ???

  extension [EE, A, B, C](
    tuple: (
      Validated[EE, A],
      Validated[EE, B],
      Validated[EE, C]
    )
  )
    def zipN: Validated[EE, (A, B, C)] = ???
    def mapN[R](f: (A, B, C) => R): Validated[EE, R] = ???

  extension [EE, A, B, C, D](
    tuple: (
      Validated[EE, A],
      Validated[EE, B],
      Validated[EE, C],
      Validated[EE, D]
    )
  )
    def zipN: Validated[EE, (A, B, C, D)] = ???
    def mapN[R](f: (A, B, C, D) => R): Validated[EE, R] = ???

  extension [EE, A, B, C, D, E](
    tuple: (
      Validated[EE, A],
      Validated[EE, B],
      Validated[EE, C],
      Validated[EE, D],
      Validated[EE, E]
    )
  )
    def zipN: Validated[EE, (A, B, C, D, E)] = ???
    def mapN[R](f: (A, B, C, D, E) => R): Validated[EE, R] = ???

  def sequence[E, A](xs: List[Validated[E, A]]): Validated[E, List[A]] = ???

  // ??? TODO: Add toValidated to Option instances

package homework3

sealed trait Validated[+E, +A]:
  def isValid: Boolean = fold(_ => false, _ => true)

  def getOrElse[B >: A](default: => B): B = fold(_ => default, a => a)

  def orElse[F >: E, B >: A](default: => Validated[F, B]): Validated[F, B] =
    fold(_ => default, _ => this)

  def zip[EE >: E, B](vb: Validated[EE, B]): Validated[EE, (A, B)] = (this, vb) match
    case (Valid(a), Valid(b)) => Valid((a, b))
    case (Invalid(errors1), Invalid(errors2)) => Invalid(errors1 ++ errors2)
    case (i @ Invalid(_), _) => i
    case (_, i @ Invalid(_)) => i

  def map[B](f: A => B): Validated[E, B] = flatMap(a => Valid(f(a)))

  def mapErrors[EE](f: Chain[E] => EE): Validated[EE, A] =
    fold(error => Invalid(f(error)), a => Valid(a))

  def zipMap[EE >: E, B, R](vb: Validated[EE, B])(f: (A, B) => R): Validated[EE, R] = zip(vb).map(f.tupled)

  def flatMap[EE >: E, B](f: A => Validated[EE, B]): Validated[EE, B] = fold(e => Invalid(e), f)
//    this match
//      case Valid(a) => f(a)
//      case i @ Invalid(_) => i

  def fold[B](fInvalid: Chain[E] => B, f: A => B): B = this match
    case Valid(a) => f(a)
    case Invalid(errors) => fInvalid(errors)

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
    def zipN: Validated[EE, (A, B)] =
      val (va, vb) = tuple
      va zip vb
    def mapN[R](f: (A, B) => R): Validated[EE, R] = tuple.zipN.map(f.tupled)

  extension [EE, A, B, C](
    tuple: (
      Validated[EE, A],
      Validated[EE, B],
      Validated[EE, C]
    )
  )
    def zipN: Validated[EE, (A, B, C)] =
      val (va, vb, vc) = tuple
      (va zip (vb, vc).zipN) map { case (a, (b, c)) =>
        (a, b, c)
      }
    def mapN[R](f: (A, B, C) => R): Validated[EE, R] = tuple.zipN.map(f.tupled)

  extension [EE, A, B, C, D](
    tuple: (
      Validated[EE, A],
      Validated[EE, B],
      Validated[EE, C],
      Validated[EE, D]
    )
  )
    def zipN: Validated[EE, (A, B, C, D)] =
      (tuple.head zip tuple.tail.zipN) map { case (a, (b, c, d)) =>
        (a, b, c, d)
      }
    def mapN[R](f: (A, B, C, D) => R): Validated[EE, R] = tuple.zipN.map(f.tupled)

  extension [EE, A, B, C, D, E](
    tuple: (
      Validated[EE, A],
      Validated[EE, B],
      Validated[EE, C],
      Validated[EE, D],
      Validated[EE, E]
    )
  )
    def zipN: Validated[EE, (A, B, C, D, E)] =
      (tuple.head zip tuple.tail.zipN) map { case (a, (b, c, d, e)) =>
        (a, b, c, d, e)
      }
    def mapN[R](f: (A, B, C, D, E) => R): Validated[EE, R] = tuple.zipN.map(f.tupled)

  def sequence[E, A](xs: List[Validated[E, A]]): Validated[E, List[A]] =
    val initial: Validated[E, List[A]] = Valid(List.empty)

    xs.foldRight(initial)((next, acc) => next.zipMap(acc)(_ :: _))

extension [A](option: Option[A])
  def toValidated[E](onEmpty: => E): Validated[E, A] =
    option.map(Valid(_)).getOrElse(Invalid(onEmpty))

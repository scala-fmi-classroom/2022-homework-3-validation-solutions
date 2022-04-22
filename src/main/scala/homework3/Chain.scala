package homework3

import scala.annotation.tailrec

enum Chain[+A]:
  case Singleton(a: A)
  case Append(left: Chain[A], right: Chain[A])

  // methods need to be final if we need them to be tailrec
  @tailrec
  final def head: A = this match
    case Singleton(a) => a
    case Append(left, _) => left.head

  def tail: Chain[A] = this match
    case Append(Singleton(_), right) => right
    case c @ Append(_, _) => c.listify.tail
    case Singleton(_) => throw new UnsupportedOperationException

  def isEmpty: Boolean = false

  def +:[B >: A](front: B): Chain[B] = ???

  def :+[B >: A](back: B): Chain[B] = ???

  def ++[B >: A](right: Chain[B]): Chain[B] = ???

  def foldLeft[B](initial: B)(f: (B, A) => B): B = ???

  def map[B](f: A => B): Chain[B] = ???

  def flatMap[B](f: A => Chain[B]): Chain[B] = ???

  def listify: Chain[A] = ???

  def foreach(f: A => Unit): Unit = foldLeft(())((_, next) => f(next))

  override def equals(that: Any): Boolean = that match
    case c: Chain[?] => ???
    case _ => false

  override def hashCode: Int = foldLeft(0)(_ * 31 + _.hashCode)

  override def toString: String = toList.mkString("Chain(", ",", ")")

  def toList: List[A] = foldLeft(List.empty[A])((acc, next) => next :: acc).reverse
  def toSet[B >: A]: Set[B] = foldLeft(Set.empty[B])((acc, next) => acc + next)

object Chain:
  def apply[A](head: A, rest: A*): Chain[A] = ???

  // Allows Chain to be used in pattern matching
  //
  // As an alternative implementation we can make Chain[A] implement Seq[A] and return it directly,
  // but that requires implementing a couple of more operations which are related to the way
  // Scala collections operate behind the scenes
  def unapplySeq[A](chain: Chain[A]): Option[Seq[A]] = Some(chain.toList)

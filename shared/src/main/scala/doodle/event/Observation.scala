package doodle
package event

sealed trait Observation[+A] {
  def map[B](f: A => B): Observation[B] =
    this match {
      case Value(a)  => Value(f(a))
      case Finished  => Finished
    }

  def flatMap[B](f: A => Observation[B]): Observation[B] =
    this match {
      case Value(a)  => f(a)
      case Finished  => Finished
    }

  def join[B, C](that: Observation[B])(f: (A, B) => C): Observation[C] =
    (this, that) match {
      case (Value(a), Value(b)) => Value(f(a, b))
      case (Finished, _)        => Finished
      case (_, Finished)        => Finished
    }
}
final case class Value[A](in: A) extends Observation[A]
final case object Finished extends Observation[Nothing]

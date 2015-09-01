package doodle
package event

sealed trait Observation[+A] {
  def map[B](f: A => B): Observation[B] =
    this match {
      case Value(a)  => Value(f(a))
      case Failed(e) => Failed(e)
      case Finished  => Finished
    }

  def flatMap[B](f: A => Observation[B]): Observation[B] =
    this match {
      case Value(a)  => f(a)
      case Failed(e) => Failed(e)
      case Finished  => Finished
    }

  def join[B, C](that: Observation[B])(f: (A, B) => C): Observation[C] =
    (this, that) match {
      case (Value(a), Value(b)) => Value(f(a, b))
      case (Failed(e), _)       => Failed(e)
      case (_, Failed(e))       => Failed(e)
      case (Finished, _)        => Finished
      case (_, Finished)        => Finished
    }
}
final case class Value[A](in: A) extends Observation[A]
final case class Failed(exn: Throwable) extends Observation[Nothing]
final case object Finished extends Observation[Nothing]

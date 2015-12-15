package doodle
package event

sealed trait Observation[+A] {
  def isFinished: Boolean =
    this match {
      case Finished => true
      case _ => false
    }

  def map[B](f: A => B): Observation[B] =
    this match {
      case Value(a)  => Value(f(a))
      case Waiting   => Waiting
      case Finished  => Finished
    }

  def flatMap[B](f: A => Observation[B]): Observation[B] =
    this match {
      case Value(a)  => f(a)
      case Waiting   => Waiting
      case Finished  => Finished
    }
}
final case class Value[A](in: A) extends Observation[A]
final case object Waiting extends Observation[Nothing]
final case object Finished extends Observation[Nothing]

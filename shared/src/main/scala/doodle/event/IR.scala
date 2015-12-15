package doodle
package event

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext,Future}

/** The stateful internal representation (IR) that we compile event streams into. */
sealed trait IR[A] {
  def eval: Observation[A]

  /** Run this for effect until it is finished */
  def run(implicit ec: ExecutionContext): Future[Unit] = {
    runFold(()){ (_, _) => () }
  }

  /** Run this until it's finished, folding over the values produced */
  def runFold[B](seed: B)(f: (B,A) => B)(implicit ec: ExecutionContext): Future[B] = {
    @tailrec
    def loop(result: B): B = {
      this.eval match {
        case Finished =>
          result
        case Waiting =>
          loop(result)
        case Value(v) =>
          loop(f(result, v))
      }
    }
    Future { loop(seed) }
  }
}
object IR {
  final case class Map[A,B](source: IR[A], f: A => B) extends IR[B] {
    def eval: Observation[B] =
      source.eval map f
  }
  final case class Scan[A,B](source: IR[A], var seed: B, f: (B,A) => B) extends IR[B] {
    def eval: Observation[B] =
      source.eval map { v =>
        val nextSeed = f(seed, v)
        seed = nextSeed
        nextSeed
      }
  }
  final case class Join[A,B](left: IR[A], right: IR[B]) extends IR[(A,B)] {
    var lastLeft: Observation[A] = Waiting
    var lastRight: Observation[B] = Waiting

    def eval: Observation[(A,B)] = {
      (left.eval, right.eval) match {
        case (Value(l), Value(r)) =>
          lastLeft = Value(l)
          lastRight = Value(r)
          Value((l, r))
        case (Value(l), Waiting) =>
          lastLeft = Value(l)
          lastRight match {
            case Value(r) =>
              Value((l, r))
            case Waiting =>
              Waiting
            case Finished =>
              Finished
          }
        case (Waiting, Value(r)) =>
          lastRight = Value(r)
          lastLeft match {
            case Value(l) =>
              Value((l, r))
            case Waiting =>
              Waiting
            case Finished =>
              Finished
          }
        case (Waiting, Waiting) =>
          if(lastLeft.isFinished || lastRight.isFinished)
            Finished
          else
            Waiting
        case (Finished, Finished) =>
          lastLeft = Finished
          lastRight = Finished
          Finished
        case (Finished, _) =>
          lastLeft = Finished
          Finished
        case (_, Finished) =>
          lastRight = Finished
          Finished
      }
    }
  }
  final case class Queue[A](queue: event.Queue[A]) extends IR[A] {
    def add(in: A): Unit =
      queue.add(in)
    def poll(): Option[A] =
      queue.poll()

    def eval: Observation[A] =
      poll() match {
        case Some(v) => Value(v)
        case None    => Waiting
      }
  }
  final case class Emit[A](var events: List[A]) extends IR[A] {
    def eval: Observation[A] =
      events match {
        case evt :: evts =>
          events = evts
          Value(evt)
        case Nil =>
          Finished
      }
  }
}

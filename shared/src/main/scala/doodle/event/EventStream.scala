package doodle
package event

import java.util.concurrent.{Semaphore,ConcurrentLinkedQueue}

sealed trait EventStream[A] {
  def map[B](f: A => B): EventStream[B] =
    Map(this, f)

  def scan[B](seed: B)(f: (B,A) => B): EventStream[B] =
    Scan(this, seed, f)

  def join[B](that: EventStream[B]): EventStream[(A,B)] =
    Join(this, that)

  def runFold[B](seed: B)(f: (B,A) => B): B = {
    val semaphore = new Semaphore(0)
    // Replace Source and Emit with Queue in the graph, and start callbacks
    // enqueing messages
    def startSources[A](node: EventStream[A]): EventStream[A] =
      node match {
        case Map(source, f) => Map(startSources(source), f)
        case Scan(source, s, f) => Scan(startSources(source), s ,f)
        case Join(l, r) => Join(startSources(l), startSources(r))
        case Emit(evts) =>
          val q = new ConcurrentLinkedQueue[A]()
          evts.foreach(e => q.add(e))
          Queue(q)
        case Callbank(h) =>
          val q = new ConcurrentLinkedQueue[A]()
          handler(a => q.add(a))
          Queue(q)
      }

    def eval(source: EventSource[A]): A =


    val network = startSources(this)

  }
}
final case class Map[A,B](source: EventStream[A], f: A => B) extends EventStream[B]
final case class Scan[A,B](source: EventStream[A], seed: B, f: (B,A) => B) extends EventStream[B]
final case class Join[A,B](left: EventStream[A], right: EventStream[B]) extends EventStream[(A,B)]
final case class Emit[A](events: List[A]) extends EventStream[A]
final case class Callback[A](handler: (A => Unit) => Unit) extends EventStream[A]
object EventStream {
  def fromCallbackHandler[A](handler: (A => Unit) => Unit) = {
    val stream = Callback(handler)
    stream
  }

  def emit[A](event: A, events: A*): EventStream[A] =
    Emit(event :: events.toList)

  /** This is the stateful internal representation (IR) that we compile event streams into. */
  object IR {
    sealed trait IR[A]
    final case class Map[A,B](source: IR[A], f: A => B) extends IR[A]
    final case class Scan[A,B](source: IR[A], var seed: B, f(B,A) => B) extends IR[A]
    final case class Join[A,B](left: IR[A], right: IR[A]) extends IR[A] {
      var lastLeft: Option[A] = None
      var lastRight: Option[B] = None
    }
    final case class Queue[A](queue: ConcurrentLinkedQueue[A]) extends IR[A]
  }
}

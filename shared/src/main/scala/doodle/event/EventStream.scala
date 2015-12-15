package doodle
package event


sealed trait EventStream[A] {
  import EventStream._

  def map[B](f: A => B): EventStream[B] =
    Map(this, f)

  def scan[B](seed: B)(f: (B,A) => B): EventStream[B] =
    Scan(this, seed, f)

  def join[B](that: EventStream[B]): EventStream[(A,B)] =
    Join(this, that)

  /** Compile this EventStream into the intermeidate representation that can be run */
  def build(implicit engine: Engine): IR[A]
}
object EventStream {
  final case class Map[A,B](source: EventStream[A], f: A => B) extends EventStream[B] {
    def build(implicit engine: Engine): IR[B] =
      IR.Map(source.build, f)
  }
  final case class Scan[A,B](source: EventStream[A], seed: B, f: (B,A) => B) extends EventStream[B] {
    def build(implicit engine: Engine): IR[B] =
      IR.Scan(source.build, seed, f)
  }
  final case class Join[A,B](left: EventStream[A], right: EventStream[B]) extends EventStream[(A,B)] {
    def build(implicit engine: Engine): IR[(A,B)] =
      IR.Join(left.build, right.build)
  }
  final case class Emit[A](events: List[A]) extends EventStream[A] {
    def build(implicit engine: Engine): IR[A] =
      IR.Emit(events)
  }
  final case class Callback[A](handler: (A => Unit) => Unit) extends EventStream[A] {
    def build(implicit engine: Engine): IR[A] = {
      val node = IR.Queue(engine.queue[A]())
      handler(a => node.add(a))
      node
    }
  }

  def fromCallbackHandler[A](handler: (A => Unit) => Unit) = {
    val stream = Callback(handler)
    stream
  }

  def emit[A](event: A, events: A*): EventStream[A] =
    Emit(event :: events.toList)
}

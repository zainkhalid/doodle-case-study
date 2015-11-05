package doodle
package event

sealed trait EventStream[A] {
  def map[B](f: A => B): EventStream[B]

  def scanLeft[B](seed: B)(f: (A, B) => B): EventStream[B]

  def join[B](that: EventStream[B]): EventStream[(A,B)]
}
object EventStream {
  def fromCallbackHandler[A](handler: (A => Unit) => Unit) = {
    val stream = new Source[A]()
    handler((evt: A) => stream.observe(evt))
    stream
  }
}
sealed trait Observer[A] {
  def observe(in: A): Unit
}
/**
  * Internal trait that provides implementation of EventStream methods in terms of a
  * mutable sequence of observers
  */
private[event] sealed trait Node[A,B] extends Observer[A] with EventStream[B] {
  import scala.collection.mutable

  val observers: mutable.ListBuffer[Observer[B]] =
    new mutable.ListBuffer()

  def update(observation: B): Unit =
    observers.foreach(_.observe(observation))

  def map[C](f: B => C): EventStream[C] = 
    withObserver(Map(f))

  def scanLeft[C](seed: C)(f: (B, C) => C): EventStream[C] =
    withObserver(ScanLeft(seed, f))

  def join[C](that: EventStream[C]): EventStream[(B,C)] = {
    val node = Join[B,C]()
    this.map(b => node.updateLeft(b))
    that.map(c => node.updateRight(c))
    node
  }

  def withObserver[C](stream: Node[B,C]): EventStream[C] = {
    observers += stream
    stream
  }
}
private[event] final case class Source[A]() extends Node[A,A] {
  def observe(in: A): Unit =
    update(in)
}
private[event] final case class Map[A,B](f: A => B) extends Node[A,B] {
  def observe(in: A): Unit =
    update(f(in))
}
private[event] final case class ScanLeft[A,B](var seed: B, f: (A, B) => B) extends Node[A,B] {
  def observe(in: A): Unit = {
    val newSeed = f(in, seed)
    seed = newSeed
    update(newSeed)
  }
}
private[event] final case class Join[A,B]() extends Node[(A,B),(A,B)] {
  var state: MutablePair[Option[A], Option[B]] = new MutablePair(None, None)
  def observe(in: (A,B)): Unit =
    update(in)

  def updateLeft(in: A) = {
    state.l = Some(in)
    state.r.foreach { r => this.update( (in, r) ) }
  }

  def updateRight(in: B) = {
    state.r = Some(in)
    state.l.foreach { l => this.update( (l, in) ) }
  }
}

private [event] class MutablePair[A,B](var l: A, var r: B)

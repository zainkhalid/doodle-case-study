package doodle
package event

sealed trait EventStream[A] {
  def map[B](f: A => B): EventStream[B]

  def foldp[B](seed: B)(f: (A, B) => B): EventStream[B]

  def join[B](that: EventStream[B]): EventStream[(A,B)]
}
object EventStream {
  def streamAndCallback[A](): (A => Unit, EventStream[A]) = {
    val stream: Source[A] =
      new Source()
    val callback: (A => Unit) =
      (evt: A) => stream.push(evt)

    (callback, stream)
  }

  def fromCallbackHandler[A](handler: (A => Unit) => Unit) = {
    val stream = new Source[A]()
    handler((evt: A) => stream.push(evt))
    stream
  }
}


trait Observer[A] {
  def observe(in:A): Unit
}

/**
  * Internal trait that provides implementation of EventStream methods in terms of a
  * mutable sequence of observers
  */
private[event] sealed trait Observable[A] extends EventStream[A] {
  import scala.collection.mutable

  val observers: mutable.ListBuffer[Observer[A]] =
    new mutable.ListBuffer()

  def update(observation: A): Unit =
    observers.foreach(_.observe(observation))

  def map[B](f: A => B): EventStream[B] = 
    withObserver(new Map(f))

  def foldp[B](seed: B)(f: (A, B) => B): EventStream[B] =
    withObserver(new FoldP(seed, f))

  def join[B](that: EventStream[B]): EventStream[(A,B)] = {
    val stream = new Join[A,B]()
    this.map(a => stream.observerA.observe(a))
    that.map(b => stream.observerB.observe(b))
    stream
  }

  def withObserver[B](stream: Observer[A] with EventStream[B]): EventStream[B] = {
    observers += stream
    stream
  }
}
private[event] final class Source[A]() extends Observable[A] {
  def push(in: A): Unit =
    update(in)
}
private[event] final class Map[A, B](f: A => B) extends Observer[A] with Observable[B] {
  def observe(in: A): Unit =
    update(f(in))
}
private[event] final class FoldP[A,B](var seed: B, f: (A, B) => B) extends Observer[A] with Observable[B] {
  def observe(in: A): Unit = {
    val newSeed = f(in, seed)
    seed = newSeed
    update(newSeed)
  }
}
private[event] final class Join[A, B]() extends Observable[(A,B)] {
  var valueA: Option[A] = None
  var valueB: Option[B] = None

  val observerA: Observer[A] =
    new Observer[A] {
      def observe(a: A): Unit = {
        valueA = Some(a)
        valueB match {
          case Some(b) =>
            update( (a,b) )
          case _ => ()
        }
      }
    } 

  val observerB: Observer[B] =
    new Observer[B] {
      def observe(b: B) = {
        valueB = Some(b)
        valueA match {
          case Some(a) =>
            update( (a,b) )
          case _ => ()
        }
      }
    }
}

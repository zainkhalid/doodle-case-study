package doodle
package event

sealed trait EventStream[A] {
  def mapObservation[B](f: Observation[A] => Observation[B]): EventStream[B]
  def foldpObservation[B](seed: Observation[B])(f: (Observation[A], Observation[B]) => Observation[B]): EventStream[B]
  def joinObservation[B, C](that: EventStream[B])(f: (Observation[A], Observation[B]) => Observation[C]): EventStream[C]

  def onObservation(f: Observation[A] => Unit)

  // Utility functions

  def map[B](f: A => B): EventStream[B] =
    mapObservation[B](_.map(f))

  def foldp[B](seed: B)(f: (A, B) => B): EventStream[B] =
    foldpObservation[B](Value(seed)){ (a, b) =>
      // Strictly speaking, only an Applicative is needed here
      for {
        theA <- a
        theB <- b
      } yield f(theA, theB)
    }

  def join[B, C](that: EventStream[B])(f: (A, B) => C): EventStream[C] =
    joinObservation[B, C](that){ (a, b) =>
      a.join(b)(f)
    }
}
object EventStream {
  def streamAndCallback[A](): (A => Unit, EventStream[A]) = {
    val stream: Source[A] =
      new Source()
    val callback: (A => Unit) =
      (evt: A) => stream.push(evt)

    (callback, stream)
  }
}


trait Observer[A] {
  def observe(in: Observation[A]): Unit
}

/**
  * Internal trait that provides implementation of EventStream methods in terms of a
  * mutable sequence of observers
  */
private[event] sealed trait Observable[A] extends EventStream[A] {
  import scala.collection.mutable

  val observers: mutable.ListBuffer[Observer[A]] =
    new mutable.ListBuffer()

  def update(observation: Observation[A]): Unit =
    observers.foreach(_.observe(observation))

  def mapObservation[B](f: Observation[A] => Observation[B]): EventStream[B] = {
    val node = new Map(f)
    observers += node
    node
  }

    def foldpObservation[B](seed: Observation[B])(f: (Observation[A], Observation[B]) => Observation[B]): EventStream[B] = {
    var currentSeed = seed
      val node = new Map( (obs: Observation[A]) => {
          val nextSeed = f(obs, currentSeed)
          currentSeed = nextSeed
          nextSeed
        })
    observers += node
    node
  }

  def joinObservation[B, C](that: EventStream[B])(f: (Observation[A], Observation[B]) => Observation[C]): EventStream[C] = {
    val node = new Join(f)
    this.onObservation(evt => node.observerA.observe(evt))
    that.onObservation(evt => node.observerB.observe(evt))
    node
  }

  def onObservation(f: Observation[A] => Unit): Unit =
    observers += new Observer[A] {
      def observe(in: Observation[A]): Unit =
        f(in)
    }
}
private[event] final class Source[A]() extends Observable[A] {
  def push(in: A): Unit =
    update(Value(in))
}
private[event] final class Map[A, B](f: Observation[A] => Observation[B]) extends Observer[A] with Observable[B] {
  def observe(in: Observation[A]): Unit =
    update(f(in))
}
private[event] final class Join[A, B, C](f: (Observation[A], Observation[B]) => Observation[C]) extends Observable[C] {
  var valueA: Option[Observation[A]] = None
  var valueB: Option[Observation[B]] = None

  val observerA: Observer[A] =
    new Observer[A] {
      def observe(in: Observation[A]): Unit = {
        valueA = Some(in)
        valueB match {
          case Some(Value(b)) =>
            update(f(in, Value(b)))
          // Don't propagate if B has already halted propagation, or has no value
          case _ => ()
        }
      }
    } 

  val observerB: Observer[B] =
    new Observer[B] {
      def observe(in: Observation[B]) = {
        valueB = Some(in)
        valueA match {
          case Some(Value(a)) =>
            update(f(Value(a), in))
          // Don't propagate if A has already halted propagation, or has no value
          case _ => ()
        }
      }
    }
}

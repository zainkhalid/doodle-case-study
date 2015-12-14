package doodle
package typeclasses

import scala.language.higherKinds

object list {
  implicit object listInstances extends Monad[List] {
    def point[A](a: A): List[A] =
      List(a)
    def map[A,B](fa: List[A])(f: A => B): List[B] =
      fa.map(f)
    def flatMap[A,B](fa: List[A])(f: A => List[B]): List[B] =
      fa.flatMap(f)
    override def zip[A,B](fa: List[A])(fb: List[B]): List[(A,B)] =
      fa.zip(fb)
  }
}

import doodle.event.EventStream
object eventStream {
  implicit object eventStreamInstances extends Applicative[EventStream] {
    def point[A](a: A): EventStream[A] = {
      val source = EventStream.source[A]
      source.observe(a)
      source
    }
    def map[A,B](fa: EventStream[A])(f: A => B): EventStream[B] =
      fa.map(f)
    override def zip[A,B](fa: EventStream[A])(fb: EventStream[B]): EventStream[(A,B)] =
      fa.join(fb)
  }
}

object Example {
  def addition[F[_] : Functor](in: F[Int]): F[Int] =
    Functor[F].map(in)(x => x + 42)

  def go = {
    val data = List(1, 2, 3)
    import list._
    println(addition(data))

    import doodle.event.EventStream
    import eventStream._
    val source = EventStream.source[Int]
    addition(source : EventStream[Int]).map(println _)
    data.map(source.observe _)
  }
}

package doodle
package typeclasses

import scala.language.higherKinds

trait Applicative[F[_]] extends Functor[F] {
  def zip[A,B](fa: F[A])(fb: F[B]): F[(A,B)]
  def point[A](a: A): F[A]
}

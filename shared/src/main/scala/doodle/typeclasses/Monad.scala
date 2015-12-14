package doodle
package typeclasses

import scala.language.higherKinds

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]

  override def zip[A,B](fa: F[A])(fb: F[B]): F[(A,B)] =
    flatMap(fa){ a =>
      map(fb){ b => (a, b)
      }
    }
}

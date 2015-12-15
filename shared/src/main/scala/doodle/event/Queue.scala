package doodle
package event

/** Scala-ized wrapper around java.util.concurrent.Queue */
trait Queue[A] {
  def add(in: A): Unit
  def poll(): Option[A]
}

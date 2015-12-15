package doodle
package event

/** Represent resources we need to run an IR on a platform */
trait Engine {
  /** Construct a queue */
  def queue[A](): Queue[A]
}

package doodle
package jvm

import doodle.event.{Engine=>EventEngine,Queue}
import java.util.concurrent.ConcurrentLinkedQueue

object Engine extends EventEngine {
  def queue[A](): Queue[A] =
    new QueueWrapper(new ConcurrentLinkedQueue())

  implicit val engine = this
}

final class QueueWrapper[A](queue: ConcurrentLinkedQueue[A]) extends Queue[A] {
  def add(in: A) =
    queue.add(in)

  def poll(): Option[A] =
    Option(queue.poll())
}

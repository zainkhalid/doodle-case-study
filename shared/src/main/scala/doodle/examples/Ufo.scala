package doodle.examples

import doodle.backend.Key
import doodle.core._
import doodle.event._

import scala.concurrent.ExecutionContext

object Ufo {
  // To run
  // Open sbt console
  // val canvas = Java2DCanvas.canvas
  // Ufo.go(canvas)
  def go(canvas: doodle.backend.Canvas)(implicit engine: Engine, ec: ExecutionContext): Unit = {
    canvas.setSize(600, 600)

    val redraw: EventStream[Double] = Canvas.animationFrameEventStream(canvas)
    val keys: EventStream[Key] = Canvas.keyDownEventStream(canvas)

    val ufo = Circle(20) fillColor (Color.red) lineColor (Color.green)

    val velocity: EventStream[Vec] =
      keys.scan(Vec.zero)((prev, key) => {
          val velocity =
            key match {
              case Key.Up    => prev + Vec(0, 1)
              case Key.Right => prev + Vec(1, 0)
              case Key.Down  => prev + Vec(0, -1)
              case Key.Left  => prev + Vec(-1, 0)
              case _         => prev
            }
          Vec(velocity.x.min(5).max(-5), velocity.y.min(5).max(-5))
        }
      )

    val location: EventStream[Vec] =
      redraw.join(velocity).map{ case(ts, v) => v }.
        scan(Vec.zero){ (velocity, prev) =>
          val location = prev + velocity
          Vec(location.x.min(300).max(-300), location.y.min(300).max(-300))
        }

    val frames = location.map(location => ufo at location)
    Canvas.animate(canvas, frames)
  }
}

package doodle.examples

import doodle.backend.Key
import doodle.core._
import doodle.event._

object Ufo {
  // To run
  // Open sbt console
  // val canvas = Java2DCanvas.canvas
  // Ufo.go(canvas)
  def go(canvas: doodle.backend.Canvas): Unit = {
    canvas.setSize(600, 600)

    val redraw = Canvas.animationFrameEventStream(canvas)
    val keys = Canvas.keyDownEventStream(canvas)

    val ufo = Circle(20) fillColor (Color.red) lineColor (Color.green)

    val location = keys.foldp(Vec.zero)((key, prev) => {
        println(s"Current location is $prev and key is $key")
        key match {
          case Key.Up    => prev + Vec(0, 1)
          case Key.Right => prev + Vec(1, 0)
          case Key.Down  => prev + Vec(0, -1)
          case Key.Left  => prev + Vec(-1, 0)
          case _         => prev
        }
      }
    )

    val frames = redraw.join(location)((ts, location) => ufo at location)
    Canvas.animate(canvas, frames)
  }
}

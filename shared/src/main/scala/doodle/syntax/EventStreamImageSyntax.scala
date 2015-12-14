package doodle.syntax

import doodle.event.{Canvas,EventStream}
import doodle.backend.Canvas
import doodle.core.{Color,Image}

trait EventStreamImageSyntax {
  implicit class EventStreamOps(val frames: EventStream[Image]) {
    def animate(implicit canvas: Canvas) =
      Canvas.animate(canvas, frames)
  }

}

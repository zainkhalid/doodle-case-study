package doodle
package event

import doodle.core.{Color, Image}

object Canvas {
  def animationFrameEventStream(canvas: backend.Canvas): EventStream[Double] = {
    val (callback, stream) = EventStream.streamAndCallback[Double]()

    canvas.setAnimationFrameCallback(callback)
    stream
  }

  def keyDownEventStream(canvas: backend.Canvas): EventStream[backend.Key] = {
    val (callback, stream) = EventStream.streamAndCallback[backend.Key]()

    canvas.setKeyDownCallback(callback)
    stream
  }

  def animate(canvas: backend.Canvas, frames: EventStream[Image]) =
    frames.map(frame => {
                 canvas.clear(Color.black)
                 frame.draw(canvas)
               })
}

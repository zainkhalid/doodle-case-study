package doodle
package event

import doodle.core.{Color, Image}

object Canvas {
  def animationFrameEventStream(canvas: backend.Canvas): EventStream[Double] = {
    EventStream.fromCallbackHandler(canvas.setAnimationFrameCallback)
  }

  def keyDownEventStream(canvas: backend.Canvas): EventStream[backend.Key] = {
    EventStream.fromCallbackHandler(canvas.setKeyDownCallback)
  }

  def animate(canvas: backend.Canvas, frames: EventStream[Image]) =
    frames.map(frame => {
                 canvas.clear(Color.black)
                 frame.draw(canvas)
               })
}

package doodle
package core

import doodle.backend.Canvas

sealed trait Image {
  def beside(right: Image): Image =
    Beside(this, right)

  def on(bottom: Image): Image =
    Overlay(this, bottom)

  def under(top: Image): Image =
    Overlay(top, this)

  def above(bottom: Image): Image =
    Above(this, bottom)

  def below(top: Image): Image =
    Above(top, this)

  def at(v: Vec): Image =
    At(v, this)

  def at(x: Double, y: Double): Image =
    At(Vec(x, y), this)

  def lineColor(color: Color): Image =
    ContextTransform(_.lineColor(color), this)

  def lineWidth(width: Double): Image =
    ContextTransform(_.lineWidth(width), this)

  def fillColor(color: Color): Image =
    ContextTransform(_.fillColor(color), this)

  /** Get a bounding box around this Image.
    *
    * Implemented here so we can cache the results, and thus avoid frequently
    * recomputing the bounding box
    */
  val boundingBox: BoundingBox =
    BoundingBox(this)

  /** Utility function */
  def draw(canvas: Canvas): Unit =
    draw(canvas, DrawingContext.whiteLines, Vec.zero)

  def draw(canvas: Canvas, context: DrawingContext, origin: Vec): Unit = {
    def doStrokeAndFill() = {
      context.fill.foreach { fill =>
        canvas.setFill(fill.color)
        canvas.fill()
      }

      context.stroke.foreach { stroke =>
        canvas.setStroke(stroke)
        canvas.stroke()
      }
    }

    this match {
      case Circle(r) =>
        canvas.circle(origin.x, origin.y, r)
        doStrokeAndFill()

      case Rectangle(w, h) =>
        canvas.rectangle(origin.x - w/2, origin.y + h/2, w, h)
        doStrokeAndFill()

      case Triangle(w, h) =>
        canvas.beginPath()
        canvas.moveTo(origin.x      , origin.y + h/2)
        canvas.lineTo(origin.x + w/2, origin.y - h/2)
        canvas.lineTo(origin.x - w/2, origin.y - h/2)
        canvas.lineTo(origin.x      , origin.y + h/2)
        canvas.endPath()
        doStrokeAndFill()

      case Overlay(t, b) =>
        b.draw(canvas, context, origin)
        t.draw(canvas, context, origin)

      case b @ Beside(l, r) =>
        val box = b.boundingBox
        val lBox = l.boundingBox
        val rBox = r.boundingBox

        val lOriginX = origin.x + box.left  + (lBox.width / 2)
        val rOriginX = origin.x + box.right - (rBox.width / 2)
        // Beside always vertically centers l and r, so we don't need
        // to calculate center ys for l and r.

        l.draw(canvas, context, Vec(lOriginX, origin.y))
        r.draw(canvas, context, Vec(rOriginX, origin.y))
      case a @ Above(t, b) =>
        val box = a.boundingBox
        val tBox = t.boundingBox
        val bBox = b.boundingBox

        val tOriginY = origin.y + box.top - (tBox.height / 2)
        val bOriginY = origin.y + box.bottom + (bBox.height / 2)

        t.draw(canvas, context, Vec(origin.x, tOriginY))
        b.draw(canvas, context, Vec(origin.x, bOriginY))
      case At(vec, i) =>
        i.draw(canvas, context, origin + vec)

      case ContextTransform(f, i) =>
        i.draw(canvas, f(context), origin)
    }
  }
}
final case class Circle(r: Double) extends Image
final case class Rectangle(w: Double, h: Double) extends Image
final case class Triangle(w: Double, h: Double) extends Image
final case class Beside(l: Image, r: Image) extends Image
final case class Above(l: Image, r: Image) extends Image
final case class Overlay(t: Image, b: Image) extends Image
final case class At(at: Vec, i: Image) extends Image
final case class ContextTransform(f: DrawingContext => DrawingContext, image: Image) extends Image

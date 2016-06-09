package doodle.core

import com.sun.org.apache.xml.internal.security.c14n.implementations.CanonicalizerBase
import doodle.backend.Canvas

// Read Chapters 5-6
// Do small exercises as needed
// Should be a larger exercise on random data/all possibilities
// Do not do animation piece - Noel will change
// Send Noel github info and repos

sealed trait Image extends Product with Serializable {

  def on(other: Image): Image = On(this, other)
  def beside(other: Image): Image = Beside(this, other)
  def above(other: Image): Image = Above(this, other)

  def draw(canvas: Canvas): Unit = draw(canvas, Point(0.0, 0.0))

  def draw(canvas: Canvas, origin: Point): Unit =
    this match {
      case Circle(r) => {
        canvas.circle(origin.x, origin.y, r)
        canvas.setStroke(Stroke(1.0, Color.gold, Line.Cap.Round, Line.Join.Miter))
        canvas.stroke()
      }

      case Rectangle(w, h) => {
        canvas.rectangle(origin.x - w / 2.0, origin.y + h / 2.0, w, h)
        canvas.setStroke(Stroke(1.0, Color.gold, Line.Cap.Round, Line.Join.Miter))
        canvas.stroke()
      }

      case Beside(left, right) => {
        val bb = this.boundingBox
        val lbb = left.boundingBox
        val rbb = right.boundingBox

        left.draw(canvas, origin.add(x = bb.left).add(x = lbb.width / 2.0))
        right.draw(canvas, origin.add(x = bb.right).subtract(x = rbb.width / 2.0))
      }

      case On(top, bottom) => {
        bottom.draw(canvas, origin)
        top.draw(canvas, origin)
      }

      case Above(top, bottom) => {
        val bb = this.boundingBox
        val bbb = bottom.boundingBox
        val tbb = top.boundingBox

        bottom.draw(canvas, origin.add(y = bb.bottom).add(y = bbb.height / 2.0))
        top.draw(canvas, origin.add(y = bb.top).subtract(y = tbb.height / 2.0))
      }
    }


  lazy val boundingBox: BoundingBox = {
    this match {
      case Circle(r) => BoundingBox(-r, r, r, -r)
      case Rectangle(w, h) => BoundingBox(-w/2.0, h/2.0, w/2.0, -h/2.0)
      case On(top, bottom) => top.boundingBox on bottom.boundingBox
      case Beside(left, right) => left.boundingBox beside right.boundingBox
      case Above(top, bottom) => top.boundingBox above bottom.boundingBox
    }
  }
}

final case class Circle(radius: Double) extends Image
final case class Rectangle(width: Double, height: Double) extends Image
final case class Beside(left: Image, right: Image) extends Image
final case class On(top: Image, bottom: Image) extends Image
final case class Above(top: Image, bottom: Image) extends Image



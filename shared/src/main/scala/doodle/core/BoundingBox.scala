package doodle.core

/**
  * Created by zainkhalid on 6/2/16.
  */
sealed case class BoundingBox(left: Double, top: Double, right: Double, bottom: Double)  {
  lazy val width = this.right - this.left
  lazy val height = this.top - this.bottom

  def beside(other: BoundingBox) =
    BoundingBox(
      -(this.width + other.width) / 2.0,
      math.max(this.top, other.top),
      (this.width + other.width) / 2.0,
      math.min(this.bottom, other.bottom)
    )

  def above(other: BoundingBox) =
    BoundingBox(
      math.min(this.left, other.left),
      (this.height + other.height) / 2.0,
      math.max(this.right, other.right),
      -(this.height + other.height) / 2.0
    )


  def on(other: BoundingBox): BoundingBox =
    BoundingBox(
      math.min(this.left, other.left),
      math.max(this.top, other.top),
      math.max(this.right, other.right),
      math.min(this.bottom, other.bottom)
  )
}

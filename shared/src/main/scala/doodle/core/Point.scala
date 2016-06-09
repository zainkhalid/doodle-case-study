package doodle.core

/**
  * Created by zainkhalid on 6/2/16.
  */
sealed case class Point(x: Double, y: Double) {
  def add(x: Double = 0.0, y: Double = 0.0) = Point(this.x + x, this.y + y)
  def subtract(x: Double = 0.0, y: Double = 0.0) = Point(this.x - x, this.y - y)
}

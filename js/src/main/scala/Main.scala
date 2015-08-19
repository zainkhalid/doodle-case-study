import doodle.core._
import doodle.js.HtmlCanvas

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom

@JSExport object Main extends JSApp {
  @JSExport def main(): Unit = {
    val canvas = HtmlCanvas.fromElementId("canvas")
    // To draw on the HTML canvas add code here
  }
}

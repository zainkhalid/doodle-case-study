package doodle
package jvm

import javax.swing.JFrame
import java.awt.event.{WindowListener, WindowEvent}

class CanvasFrame extends JFrame {
  val panel = new CanvasPanel()

  getContentPane().add(panel)
  panel.setFocusable(true)
  pack()

  this.addWindowListener(new WindowListener {
                           def windowClosed(e: WindowEvent): Unit =
                             panel.close()
                           def windowClosing(e: WindowEvent): Unit =
                             panel.close()
                           def windowActivated(e: WindowEvent): Unit = ()
                           def windowDeactivated(e: WindowEvent): Unit = ()
                           def windowDeiconified(e: WindowEvent): Unit = ()
                           def windowIconified(e: WindowEvent): Unit = ()
                           def windowOpened(e: WindowEvent): Unit = ()
                         })
}

package nesscala.gui

import java.awt.Color
import javax.swing.JFrame

import org.scalatest.FlatSpec

/**
 * Created by chenyan06 on 2015/6/5.
 */
class DisplaySpec extends FlatSpec {

  "Display" should "draw a image" in {
    val frame = new JFrame
    val display = new Display

    display.screen.setPixel(100, 100, Color.RED)
    display.screen.setPixel(100, 200, Color.BLUE)
    display.pixelSize = 2

    frame.add(display)
    frame.setVisible(true)
  }

}

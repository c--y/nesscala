package nesscala.gui

import java.awt.Color
import javax.swing.JFrame

import scala.util.Random

/**
 * Created by chenyan06 on 2015/6/5.
 */
object DisplayTest {

  class Updater(val display: Display) extends Runnable {
    val colors = Array[Color](Color.RED, Color.BLUE, Color.GREEN)

    override def run(): Unit = {
      while (true) {
        Thread.sleep(16)
        val row = Random.nextInt(239)
        val col = Random.nextInt(255)
        val c = colors(Random.nextInt(colors.length))
        display.screen.setPixel(row, col, c)
        display.screen.setPixel(row + 1, col + 1, c)

        display.repaint()
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val frame = new JFrame
    frame.setBounds(0, 0, 400, 600)
    val display = new Display

    display.screen.setPixel(100, 100, Color.RED)
    display.screen.setPixel(100, 200, Color.BLUE)
    display.setPixelSize(3)

    val up = new Updater(display)
    new Thread(up).start()

    frame.add(display)
    frame.setVisible(true)
  }

}

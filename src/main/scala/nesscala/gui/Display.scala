package nesscala.gui

import java.awt.{Color, Graphics}
import java.awt.image.BufferedImage
import javax.swing.{JPanel}

import scala.collection.mutable

/**
 * Created by chenyan06 on 2015/6/5.
 */
class Display extends JPanel {

  val chan = new mutable.Queue[ScreenBuffer]()

  var buffer = new BufferedImage(240, 256, BufferedImage.TYPE_4BYTE_ABGR)

  var bufferGraphics = buffer.createGraphics()

  var pixelSize: Int = 1

  var screen = new ScreenBuffer

  override def paintComponent(g: Graphics): Unit = {
    paintBuffer(buffer, screen)
    g.drawImage(buffer, 0, 0, buffer.getWidth, buffer.getHeight(), null)
  }

  def paintBuffer(buf: BufferedImage, screen: ScreenBuffer): Unit = {
    val g = buf.getGraphics()

    for (i <- 0 until screen.height; j <- 0 until screen.width) {
      val color = screen.pixel(i, j)
      if (color == null) g.setColor(Color.WHITE) else g.setColor(color)
      //g.setColor(screen.pixel(i, j))
      g.fillRect(i * pixelSize, j * pixelSize, pixelSize, pixelSize)
    }
  }

  def setPixelSize(size: Int): Unit = {
    require(size > 1)

    pixelSize = size
    buffer = new BufferedImage(240 * size, 256 * size, BufferedImage.TYPE_4BYTE_ABGR)
    bufferGraphics.dispose()
    bufferGraphics = buffer.createGraphics()
  }

}

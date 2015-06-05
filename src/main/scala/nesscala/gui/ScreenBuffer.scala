package nesscala.gui

import java.awt.Color
import java.awt.image.BufferedImage

/**
 * Created by chenyan06 on 2015/6/5.
 */
class ScreenBuffer() {

  val width = 256

  val height = 240

  val data = Array.fill(240){new Array[Color](256)}

  def pixel(row: Int, col: Int) =
    data(row)(col)

  def setPixel(row: Int, col: Int, c: Color) =
    data(row)(col) = c

}

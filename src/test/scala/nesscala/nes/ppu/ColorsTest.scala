package nesscala.nes.ppu

import javax.swing.{JPanel, JFrame}

/**
 * 测试色盘
 *
 * Created by chenyan on 15-6-7.
 */
object ColorsTest {

  def main(args: Array[String]): Unit = {
    val frame = new JFrame()
    val panel = new JPanel()
    frame.setBounds(0, 0, 1000, 800)
    frame.add(panel)
    frame.setVisible(true)

    val g = panel.getGraphics
    val startX = 100
    val startY = 100
    var blockSize = 30

    for (i <- 0 until 4) {
      for (j <- 0 until 16) {
        g.setColor(Colors.table(i * 16 + j))
        g.fillRect(startX + j * blockSize, startY + i * blockSize, blockSize, blockSize)
      }
    }

  }

}

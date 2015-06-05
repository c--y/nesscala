package nesscala.nes.ppu

/**
 * Created by chenyan06 on 2015/6/5.
 */
class PatternTable {

  val TileSize = 16

  val store = new Array[Byte](0x1000)

  def read(address: Int): Int = {
    require(true)
    0
  }

  def write(address: Int, value: Byte): Unit = {
    require(true)

  }

  def tile(address: Int): Array[Byte] = {
    require(true)
    store.slice(address, address + TileSize)
  }

  def dumpTile(tile: Array[Byte]): String = {
    require(tile.length == TileSize)

    val sb = new StringBuilder

    for (i <- 0 until 8) {
      val lowBits = tile(i)
      val highBits = tile(i + 8)
      for (j <- 0 until 8) {
        var v = if (((lowBits << j) & 0x80) > 0) 1 else 0
        v += (if (((highBits << j) & 0x80) > 0) 2 else 0)
        sb ++= (if (v == 0) " " else v.toString)
      }
      sb ++= "\n"
    }
    sb.toString()
  }

}

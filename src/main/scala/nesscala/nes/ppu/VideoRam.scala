package nesscala.nes.ppu

/**
 * Created by chenyan on 15-6-2.
 */
class VideoRam {

  // Actual 16kb
  // val units = new Array[Byte](0x4000)
  val nameTables = new NameTables

  val patternTables = Array.fill(2){new PatternTable}

  def read(address: Int): Int = {
    0
  }

  def write(address: Int, value: Byte): Unit = {

  }

}

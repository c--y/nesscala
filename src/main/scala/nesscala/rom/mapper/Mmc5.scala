package nesscala.rom.mapper

import nesscala.rom.{NesFile, Mapper}

/**
 * Created by chenyan on 15-6-8.
 */
class Mmc5
(val nesFile: NesFile) extends Mapper {

  val extendedRam = new Array[Byte](0x400)

  // 0x5100
  var prgMode: Byte = 0

  // 0x5101
  var chrMode: Byte = 0

  // 0x5104
  var extendedRamMode: Byte = 0

  // 0x5105
  var nameTableMapping: Byte = 0

  override def read(address: Int): Int = ???

  override def getType(): Symbol = 'mmc5

  override def writeVram(address: Int, value: Byte): Unit = ???

  override def write(address: Int, value: Byte): Unit = ???

  override def readVram(address: Int): Int = ???

  override def readTile(address: Int): Array[Byte] = ???
}

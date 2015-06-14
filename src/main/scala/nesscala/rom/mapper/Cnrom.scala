package nesscala.rom.mapper

import nesscala.rom.{NesFile, Mapper}

/**
 * Cnrom switches allowed swapping of CHR-ROM banks but not PRG-ROM.
 *
 * Created by chenyan on 15-5-31.
 */
class Cnrom(val nesFile: NesFile) extends Mapper{

  override def read(address: Int): Int = {
    1
  }

  override def writeVram(address: Int, value: Byte): Unit = ???

  override def write(address: Int, value: Byte): Unit = ???

  override def readVram(address: Int): Int = ???

  override def step(): Unit = ???

  override def getType(): Symbol = 'cnrom

  override def readTile(address: Int): Array[Byte] = ???
}

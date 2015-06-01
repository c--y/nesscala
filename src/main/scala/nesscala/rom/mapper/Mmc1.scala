package nesscala.rom.mapper

import nesscala.rom.{Mapper, NesFile}

/**
 * MMC1 allowed switching of both PRG-ROM and CHR-ROM banks. The chip also
 * allowed changes to name table mirroring and had support for saving to a RAM chip. The
 * maximum number of 16 KB PRG-ROM banks using MMC1 is 8. MMC1 was the most
 * used memory mapper, being used by a variety of games including Metroid and The
 * Legend of Zelda
 *
 * Created by chenyan on 15-5-31.
 */
class Mmc1(val nesFile: NesFile) extends Mapper {

  override def read(address: Int): Int = ???

  override def writeVram(address: Int, value: Byte): Unit = ???

  override def write(address: Int, value: Byte): Unit = ???

  override def readVram(address: Int): Int = ???

  override def step(): Unit = ???
}

package nesscala.rom.mapper

import nesscala.rom.{Mapper, NesFile}

/**
 * MMC3 allowed switching of both PRG-ROM and CHR-ROM banks. The chip also
 * allowed for selective screen scrolling, that is allowing part of the screen to move while
 * part remains stationary, and was capable of generating IRQs. The maximum number of
 * 16 KB PRG-ROM banks using MMC3 is 32 [27]. MMC3 was the second most used chip,
 * used by games including Super Mario Bros. 2 and Super Mario Bros. 3
 * Created by chenyan on 15-5-31.
 */
class Mmc3(val nesFile: NesFile) extends Mapper {

  override def read(address: Int): Int = ???

  override def writeVram(address: Int, value: Byte): Unit = ???

  override def write(address: Int, value: Byte): Unit = ???

  override def readVram(address: Int): Int = ???

  override def step(): Unit = ???

  override def getType(): Symbol = 'mmc3
}

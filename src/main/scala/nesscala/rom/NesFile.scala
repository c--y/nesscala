package nesscala.rom

import nesscala.rom.mapper.{Mmc3, Cnrom, Unrom, Mmc1}

/**
 * Created by chenyan on 15-5-30.
 */
class NesFile(
               val romControl: RomControl,
               val prgBanks: Array[Array[Byte]],
               val chrBanks: Array[Array[Byte]],
               val trainer: Array[Byte]
               ) {

  def getMapper(): Mapper = {
    romControl.mapperType match {
      case 0 => null
      case 1 => new Mmc1(this)
      case 2 => new Unrom(this)
      case 3 => new Cnrom(this)
      case 4 => new Mmc3(this)
      case _ => throw new RuntimeException
    }
  }
}

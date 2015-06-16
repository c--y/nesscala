package nesscala.rom.mapper

import nesscala.rom.{NesFile, Mapper}
import nesscala.util.IntUtils

/**
 * INES Mapper 000
 *
 * Created by chenyan on 15-6-14.
 */
class Nrom(val nesFile: NesFile) extends Mapper {

  // Load from Nes file
  val prgBanks = nesFile.prgBanks

  val vromBanks =
    if (nesFile.chrBanks.length == 0)
      Array.fill(2)(new Array[Byte](0x1000))
    else
      new Array[Array[Byte]](2 * nesFile.chrBanks.length)

  // Video ram 以0x1000为单位
  if (nesFile.chrBanks.length > 0) {
    for (i <- 0 until vromBanks.length) {
      val offset = i % 2
      vromBanks(i) = nesFile.chrBanks(i / 2).slice(offset, offset + 0x1000)
    }
  }

  override def getType(): Symbol = 'nrom

  override def read(address: Int): Int =
    IntUtils.toUnsigned(if (address >= 0xc000) prgBanks(prgBanks.length - 1)(address & 0x3fff) else prgBanks(0)(address & 0x3fff))

  override def write(address: Int, value: Byte): Unit = {
    // Do nothing
  }

  override def readVram(address: Int): Int =
    IntUtils.toUnsigned(if (address >= 0x1000) vromBanks(vromBanks.length - 1)(address & 0xfff) else vromBanks(0)(address & 0xfff))

  override def writeVram(address: Int, value: Byte): Unit =
    if (address >= 0x1000)
      vromBanks(vromBanks.length - 1)(address & 0xfff) = value
    else
      vromBanks(0)(address & 0xfff) = value

  override def readTile(address: Int): Array[Byte] =
    if (address >= 0x1000)
      vromBanks(vromBanks.length - 1).slice(address & 0xfff, address + 16)
    else
      vromBanks(0).slice(address & 0xfff, address + 16)
}

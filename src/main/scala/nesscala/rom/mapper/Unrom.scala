package nesscala.rom.mapper

import nesscala.rom.{Mapper, NesFile}
import nesscala.util.IntUtils

/**
 * Unrom switches were the first chips to allow bank switching of NES games.
 * Unrom only allowed switching of PRG-ROM banks.
 * The maximum number of 16kb PRG-ROM banks using Unrom is 8.
 *
 * Created by chenyan on 15-5-31.
 */
class Unrom(val nesFile: NesFile) extends Mapper {

  var currentBank = 0

  // address all are unsigned short
  override def read(address: Int): Int = {
    if (address >= 0xc000) {
      return IntUtils.toUnsigned(nesFile.prgBanks(nesFile.prgBanks.length - 1)(address & 0x3fff))
    }
    return IntUtils.toUnsigned(nesFile.prgBanks(currentBank)(address & 0x3fff))
  }

  override def write(address: Int, value: Byte): Unit = {
    currentBank = address & 0x7
  }

  override def writeVram(address: Int, value: Byte): Unit = {
    if (address >= 0x1000) {
      nesFile.chrBanks(nesFile.chrBanks.length - 1)(address & 0xfff) = value
    } else {
      nesFile.chrBanks(0)(address & 0xfff) = value
    }
  }

  override def readVram(address: Int): Int = {
    if (address >= 0x1000) {
      return IntUtils.toUnsigned(nesFile.chrBanks(nesFile.chrBanks.length - 1)(address & 0xfff))
    }
    return IntUtils.toUnsigned(nesFile.chrBanks(0)(address & 0xfff))
  }

  override def step(): Unit = {}

  override def getType(): Symbol = 'unrom

  override def readTile(address: Int): Array[Byte] = ???
}

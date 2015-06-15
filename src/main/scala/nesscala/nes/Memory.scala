package nesscala.nes

import nesscala.util.IntUtils

/**
 * 内存
 *
 * Created by chenyan on 15-5-31.
 */
class Memory() {

  val MemorySize = 0x100000

  val store: Array[Byte] = new Array(MemorySize)

  def read(address: Int): Int =
    address match {
      case _ if address > 0x2008 && address < 0x4000 =>
        IntUtils.toUnsigned(M.ppu.readRegister(0x2000 + address % 0x8))
      case _ if address <= 0x2007 && address >= 0x2000 =>
        IntUtils.toUnsigned(M.ppu.readRegister(address))
      case 0x4016 =>
        // TODO Pads[0].read
        0
      case 0x4017 =>
        // TODO Pads[1].read
        0
      case _ if (address & 0xf000) == 0x4000 =>
        // TODO apu.readRegister()
        0
      case _ if address >= 0x8000 && address <= 0xffff =>
        M.rom.read(address)
      case _ if address >= 0x5100 && address <= 0x6000 =>
        // TODO mmc5
        0

      case _ =>
        IntUtils.toUnsigned(store(address))

    }

  def write(address: Int, value: Byte) =
    address match {
      case _ if address >= 0x2000 && address <= 0x2007 =>
        M.ppu.writeRegister(address, value)
      case 0x4014 =>
        M.ppu.writeRegister(address, value)
        // XXX need store in ram?
      case 0x4016 =>
        // TODO pads[0].write
      case 0x4017 =>
        // TODO pads[1].write
      case _ if (address & 0xf000) == 0x4000 =>
        // TODO apu.writeRegister
      case _ if address >= 0x8000 && address <= 0xffff =>
        M.rom.write(address, value)
      case _ if address >= 0x5100 && address <= 0x6000 =>
        // TODO mmc5
      case _ =>
        store(address) = value
    }
}

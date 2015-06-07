package nesscala.nes.ppu

import nesscala.util.BitUtils

/**
 *  PPUCTRL
 *
 *  7 6 5 4 3 2 1 0
 *  V P H B S I N N
 *
 *  7, V, generate an NMI at the start of the Vblank interval.(0: off, 1: on)
 *  6, P, ppu master/slave select.(0: read backdrop from EXT pins, 1: output color on EXT pins)
 *  5, H, sprite size.(0: 8*8, 1: 8*16)
 *  4, B, background pattern table address.(0: 0x0000, 1: 0x1000)
 *  3, S, sprite pattern table address for 8*8 sprites.(0:0x0000, 1: 0x1000, ignored in 8*16 mode)
 *  2, I, vram address increment per CPU read/write of PPUDATA.(0: add 1, going across, 1: add 32, going down)
 *  1, N
 *  0, N  base nametable address (0: 0x2000, 1:0x2400, 2:0x2800, 3: 0x2c00)
 *
 * Created by chenyan on 15-6-2.
 */
class ControlRegister {

  // value
  var v: Byte = 0

  def baseNametable(): Int =
    v & 0x03

  def baseNametableAddress(): Int = (v & 0x03) match {
    case 0x00 => 0x2000
    case 0x01 => 0x2400
    case 0x02 => 0x2800
    case 0x03 => 0x2c00
    case _ => throw new RuntimeException("Invalid base nametable index")
  }

  def setBaseNametable(idx: Int): Unit = {
    v = ((v | 0x03) & (idx & 0x03)).toByte
  }

  // Scrolling
  //  bit 0 : add 256 to x scroll position
  //  bit 1 : add 240 to y scroll position
  def scrollCoordinates(): (Boolean, Boolean) =
    (BitUtils.isSet(v, 0), BitUtils.isSet(v, 1))

  def addressIncrement(): Boolean =
    BitUtils.isSet(v, 2)

  def setAddressIncrement(value: Boolean) =
    v = BitUtils.setBit(v, 2, value)

  def spritePattern(): Boolean =
    BitUtils.isSet(v, 3)

  def spritePatternAddress(): Int = spritePattern() match {
    case true => 0x1000
    case false => 0x0
  }

  def setSpritePattern(value: Boolean) =
    v = BitUtils.setBit(v, 3, value)

  def backgroundPattern(): Boolean =
    BitUtils.isSet(v, 4)

  // Ignored in 8 * 16 mode
  def backgroundPatternAddress(): Int = backgroundPattern() match {
    case true => 0x1000
    case false => 0x0
  }

  def setBackgroundPattern(value: Boolean) =
    v = BitUtils.setBit(v, 4, value)

  def spriteSize(): Boolean =
    BitUtils.isSet(v, 5)

  def setSpriteSize(value: Boolean) =
    v = BitUtils.setBit(v, 5, value)

  def masterSlave(): Boolean =
    BitUtils.isSet(v, 6)

  def setMasterSlave(value: Boolean) =
    BitUtils.setBit(v, 6, value)

  def nmiOnVblank(): Boolean =
    BitUtils.isSet(v, 7)

  def setNmiOnVblank(value: Boolean) =
    BitUtils.setBit(v, 7, value)


}

package nesscala.nes

import nesscala.nes.ppu._

/**
 *
 * Common Name	Address	Bits	Notes
 * -------------------------------------
 * PPUCTRL	$2000	VPHB SINN	NMI enable (V), PPU master/slave (P), sprite height (H), background tile select (B), sprite tile select (S), increment mode (I), nametable select (NN)
 * PPUMASK	$2001	BGRs bMmG	color emphasis (BGR), sprite enable (s), background enable (b), sprite left column enable (M), background left column enable (m), greyscale (G)
 * PPUSTATUS	$2002	VSO- ----	vblank (V), sprite 0 hit (S), sprite overflow (O), read resets write pair for $2005/2006
 * OAMADDR	$2003	aaaa aaaa	OAM read/write address
 * OAMMDATA	$2004	dddd dddd	OAM data read/write
 * PPUSCROLL	$2005	xxxx xxxx	fine scroll position (two writes: X, Y)
 * PPUADDR	$2006	aaaa aaaa	PPU read/write address (two writes: MSB, LSB)
 * PPUDATA	$2007	dddd dddd	PPU data read/write
 * OAMDMA	$4014	aaaa aaaa	OAM DMA high address
 *
 * Created by chenyan on 15-5-30.
 */
class Ppu(val cpu: Cpu) {

  // 16 bytes in pattern table
  val TileSize = 16

  // cpu use these registers to read/write ppu
  // 0x2000
  val rControl = new ControlRegister

  // 0x2001
  val rMask = new MaskRegister

  // 0x2002
  val rStatus = new StatusRegister

  // 0x2003
  var rOamAddr: Int = 0

  // 0x2004
  var rOamData: Byte = 0

  var writeLatch: Boolean = false

  // 0x2005, Write twice
  var rScroll: Byte = 0

  // 0x2006, Write twice
  var rAddress: Int = 0

  // 两次写过程中, 第一次写后的值
  var rLatchTemp: Int = 0

  // 0x2007
  var rData: Byte = 0

  // 4 NameTables
  val nameTables = new NameTables

  // 2 PatternTables in video ram
  val patternTables = Array.fill(2) {new Array[Byte](0x1000)}

  var oam = new SpriteRam(64)

  var secondaryOam = new SpriteRam(8)

  var cycle = 0
  // current scanLine
  var scanLine = 0

  var frameCount = 0

  def writeOamAddress(value: Byte): Unit = {
    this.rOamAddr = value
  }

  def writeOamData(value: Byte): Unit = {
    oam.write(this.rOamAddr, value)
    this.rOamAddr += 1
    this.rOamAddr %= 0x100
  }

  def readOamData(): Byte =
    oam.read(this.rOamAddr)

  def writeDma(v: Byte): Unit = {
    cpu.cycles = 512

    val address = v * 0x100
    for (i <- 0 until 0x100) {
      val v = cpu.ram.read(address + i)
      oam.write(i, v.toByte)
    }
  }

  def writeScroll(value: Byte): Unit = {
    if (writeLatch) {
      rLatchTemp &= 0x7fe0
      rLatchTemp |= ((value & 0xf8) >> 3)
    } else {
      rLatchTemp &= 0xc1f
      rLatchTemp |= (((value & 0xf8) << 2) | ((value & 0x07) << 12))
    }

    writeLatch = !writeLatch
  }

  def writeAddress(value: Byte): Unit = {
    if (writeLatch) {
      rLatchTemp &= 0xff
      rLatchTemp |= ((value & 0x3f) << 8)
    } else {
      rLatchTemp &= 0x7f00
      rLatchTemp |= value
      rAddress = rLatchTemp
    }
    writeLatch = !writeLatch
  }

  def writeData(value: Byte): Unit = {
    rAddress match {
      case _ if rAddress > 0x3000 => {}
      case _ if rAddress >= 0x2000 && rAddress < 0x3000 => {}
      case _ if rAddress < 0x2000 => {

      }
      case _ => {}
    }

    rAddress += 1
  }

  def readData(): Byte = {
    0
  }

  def incAddress(): Unit = {
    if (rControl.addressIncrement())
      rAddress += 0x20
    else
      rAddress += 0x01
  }

  /**
   * 341 ppu's cycles(or 341/3 cpu's cycles) make up the time of a typical scanline
   *
   * One frame consists of 262 scanlines.
   */
  def runStep(): Unit = {
    scanLine match {
      // Post-render line
      case 240 =>
        if (cycle == 1) {

        }
      // End of vblank
      case 260 =>
        cycle match {
          case 1 =>
            rStatus.setInVblank(false)
          // One frame finished
          case 341 => {
            scanLine = -1
            cycle = 1
            frameCount += 1
            return
          }
        }
      case _ if (scanLine >= 0 && scanLine < 240) =>
        cycle match {
          case 254 => if (rMask.background()) {
              // Mmc5
              // renderTileRow()
            }

            if (rMask.sprite()) {
              // Mmc5
              evalSprites(scanLine)
            }
          case 256 => if (rMask.background()) {
              // updateEndScanlineRegisters

            }
          case 260 => if (rControl.spritePattern() && !rControl.backgroundPattern()) {
              // Mmc3 hook
            }
        }
      // Pre-render scanLine
      case -1 =>
        cycle match {
          case 1 =>
            rStatus.setSprite0Hit(false)
            rStatus.setSpriteOverflow(false)
          case 304 =>
            if (rMask.background() || rMask.sprite()) rAddress = rLatchTemp
      }
    }

    //
    if (cycle == 341) {
      cycle = 0
      scanLine += 1

      // Mmc5
    }
  }

  /**
   * Evaluate a scanLine
   *
   * @param line
   */
  def evalSprites(line: Int): Unit = {
    var sprCount = 0
    var sprHeight = if (rControl.spriteSize()) 16 else 8



  }

  /**
   * Dump a tile in pattern tables.
   *
   * @param tile
   * @return
   */
  def dumpTile(tile: Array[Byte]): String = {
    require(tile.length == TileSize)

    val sb = new StringBuilder

    for (i <- 0 until 8) {
      val lowBits = tile(i)
      val highBits = tile(i + 8)
      for (j <- 0 until 8) {
        var v = if (((lowBits << j) & 0x80) > 0) 1 else 0
        v += (if (((highBits << j) & 0x80) > 0) 2 else 0)
        sb ++= (if (v == 0) " " else v.toString)
      }
      sb ++= "\n"
    }
    sb.toString()
  }
}

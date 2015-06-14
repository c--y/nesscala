package nesscala.nes

import util.control.Breaks._
import java.awt.Color

import nesscala.nes.cpu.InterruptNmi
import nesscala.nes.ppu._
import nesscala.rom.Mapper
import nesscala.util.IntUtils

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
class Ppu() {

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

  // Address latch, used by PPUADDR & PPUSCROLL
  var writeLatch: Boolean = false

  // 0x2005, Write twice
  var rScroll: Byte = 0

  // 像素维度的x坐标
  var rFineX: Int = 0

  // 0x2006, Write twice
  var rAddress: Int = 0

  // 两次写过程中, 第一次写后的值
  var rLatchTemp: Int = 0

  // 0x2007
  var rData: Byte = 0

  // 两个16bit移位寄存器
  var rLowShift: Short = 0

  var rHighShift: Short = 0

  // PPUDATA read buffer (post-fetch)
  var readBuffer: Byte = 0

  // 4 NameTables
  val nameTables = new NameTables

  // 2 PatternTables in video ram
  val patternTables = Array.fill(2) {new Array[Byte](0x1000)}

  // 0x0 ~ 0x10, Image palette
  // 0x10 ~ 0x20, Sprite palette
  val palettes = new Array[Byte](0x20)

  var oam = new SpriteRam(64)

  var secondaryOam = new SpriteRam(8)

  var cycle = 0
  // current scanLine
  var scanLine = 0

  var frameCount = 0

  // flags
  var suppressNmi = false

  var suppressVbl = false

  // buffer
  val frameBuffer = Array.fill(0xf000) {new Pixel}

  def readRegister(address: Int): Byte =
    (address & 0x7) match {
      case 0x2 =>
        readStatus().v
      case 0x4 =>
        readOamData()
      case 0x7 =>
        readData()
      case _ => 0
    }

  def writeRegister(address: Int, value: Byte): Unit =
    (address & 0x7) match {
      case 0x0 =>
        rControl.v = value
        // TODO rLatchTemp 需要做相应调整?
      case 0x1 =>
        rMask.v = value
      case 0x3 =>
        rOamAddr = value
      case 0x4 =>
        writeOamData(value)
      case 0x5 =>
        writeScroll(value)
      case 0x6 =>
        writeAddress(value)
      case 0x7 =>
        writeData(value)
    }

  def readStatus(): StatusRegister = {
    writeLatch = true
    // Post-render scanline (240)
    // The PPU just idles during this scanline.
    // Even though accessing PPU memory from the program would be safe here,
    //  the VBlank flag isn't set until after this scanline.
    if (cycle == 1 && scanLine == 240) {
      suppressNmi = true
      suppressVbl = true
    } else {
      suppressNmi = false
      suppressVbl = false
      rStatus.setInVblank(false)
    }
    rStatus
  }

  def writeOamData(value: Byte): Unit = {
    oam.write(this.rOamAddr, value)
    this.rOamAddr += 1
    this.rOamAddr %= 0x100
  }

  def readOamData(): Byte =
    oam.read(this.rOamAddr)

  def writeDma(v: Byte): Unit = {
    M.cpu.cycles = 512

    val address = v * 0x100
    for (i <- 0 until 0x100) {
      val v = M.ram.read(address + i)
      oam.write(i, v.toByte)
    }
  }

  def writeScroll(value: Byte): Unit = {
    if (writeLatch) {
      rLatchTemp &= 0x7fe0
      rLatchTemp |= ((value & 0xf8) >> 3)
      rFineX = value & 0x07
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

  def spritePatternAddress(index: Int): Int =
    if (rControl.spriteSize()) {
      if ((index & 0x01) != 0)
        0x1000 | (index >> 1) * 0x20
      else
        (index >> 1) * 0x20
    } else {
      (index * 0x10) * rControl.spritePatternAddress()
    }


  def backgroundPatternAddress(index: Int): Int =
    rControl.backgroundPatternAddress() | (index << 4) | (rAddress >> 12)

  /**
   *         Programmer Memory Map
   *  +---------+-------+-------+--------------------+
   *  | Address | Size  | Flags | Description        |
   *  +---------+-------+-------+--------------------+
   *  | $0000   | $1000 | C     | Pattern Table #0   |
   *  | $1000   | $1000 | C     | Pattern Table #1   |
   *  | $2000   | $3C0  |       | Name Table #0      |
   *  | $23C0   | $40   |  N    | Attribute Table #0 |
   *  | $2400   | $3C0  |  N    | Name Table #1      |
   *  | $27C0   | $40   |  N    | Attribute Table #1 |
   *  | $2800   | $3C0  |  N    | Name Table #2      |
   *  | $2BC0   | $40   |  N    | Attribute Table #2 |
   *  | $2C00   | $3C0  |  N    | Name Table #3      |
   *  | $2FC0   | $40   |  N    | Attribute Table #3 |
   *  | $3000   | $F00  |   R   |                    |
   *  | $3F00   | $10   |       | Image Palette #1   |
   *  | $3F10   | $10   |       | Sprite Palette #1  |
   *  | $3F20   | $E0   |    P  |                    |
   *  | $4000   | $C000 |     F |                    |
   *  +---------+-------+-------+--------------------+
   *
   *                      C = Possibly CHR-ROM
   *                      N = Mirrored (see Subsection G)
   *                      P = Mirrored (see Subsection H)
   *                      R = Mirror of $2000-2EFF (VRAM)
   *                      F = Mirror of $0000-3FFF (VRAM)
   */

  def writeData(value: Byte): Unit = {
    rAddress match {
      // Mirrored Address
      case _ if rAddress >= 0x3000 =>
        writeMirroredData(rAddress, value)
      // NameTables mirror
      case _ if rAddress >= 0x2000 && rAddress < 0x3000 =>
        nameTables.write(rAddress, value)
      // PattenTables
      case _ if rAddress < 0x2000 => {
        M.rom.writeVram(rAddress, value)
        // TODO mmc2
      }
      case _ =>
    }

    incAddress()
  }

  def writeMirroredData(address: Int, value: Byte): Unit =
    address match {
      // Palettes
      case _ if address > 0x3f00 =>
        palettes(if ((address & 0xf) == 0) 0 else (address & 0x1f)) = value
      // NameTables (0x3000 ~ 0x3f00)
      case _ =>
        nameTables.write(address - 0x1000, value)
    }

  /**
   * Read data from 0x2007 port
   *
   * Using one-byte read buffer.
   *
   * @return
   */
  def readData(): Byte = {
    var result: Byte = 0
    if (rAddress >= 0x2000 && rAddress < 0x3000) {
      result = readBuffer
      readBuffer = nameTables.read(rAddress).toByte
    } else if (rAddress < 0x3f00) {
      result = readBuffer
      if (rAddress < 0x2000) {
        readBuffer = M.rom.readVram(rAddress).toByte
        // Mmc2
      } else {
        //
      }
    } else { // >= 0x3f00
      val bufferAddress = rAddress - 0x1000
      bufferAddress match {
        case _ if bufferAddress >= 0x2000 && bufferAddress < 0x3000 =>
          readBuffer = nameTables.read(bufferAddress).toByte
        case _ => {} // TODO
      }

      result = palettes(if ((rAddress & 0xf) == 0) 0 else (rAddress & 0x1f))
      if (rAddress < 0x2000) {
        // TODO Mmc2
      }
    }

    incAddress()
    result
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
          if (!suppressVbl) rStatus.setInVblank(true)
          if (rControl.nmiOnVblank() && !suppressNmi) M.cpu.requestInterrupt(InterruptNmi)
          raster()
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
          case 254 =>
            if (rMask.background()) {
              // Mmc5
              evalBackground()
            }

            if (rMask.sprite()) {
              // Mmc5
              evalSprites(scanLine)
            }
          case 256 => if (rMask.background())
            updateEndScanLine()
          case 260 => if (rControl.spritePattern() && !rControl.backgroundPattern()) {
              //TODO Mmc3 hook
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

      // TODO Mmc5
    }
  }

  /**
   * 读取背景
   *
   * @return
   */
  def readBackgroundTile(): (Short, Short, Int) = {
    val attr = nameTables.readAttr(rAddress)
    val pIndex = nameTables.read(rAddress)
    val tileAddress = backgroundPatternAddress(pIndex)

    if ((rAddress & 0x1f) == 0x1f)
      rAddress ^= 0x41f
    else
      rAddress += 1

    // 每个tile由16字节组成
    (M.rom.readVram(tileAddress).toShort, M.rom.readVram(tileAddress + 8).toShort, attr)
  }

  def readBackgroundPalette(attr: Int, pixelIndex: Int): Byte = {
    if (pixelIndex == 0) palettes(0) else palettes(attr + pixelIndex)
  }

  /**
   * Evaluate the background of a scanline.
   */
  def evalBackground(): Unit = {
    var (low, high, attr) = readBackgroundTile()
    rLowShift = low
    rHighShift = high
    var attrTmp = 0

    readBackgroundTile() match {
      case (a, b, c) => low = a; high = b; attrTmp = c
    }
    rLowShift = ((rLowShift << 8) | low).toShort
    rHighShift = ((rHighShift << 8) | high).toShort

    for (i <- 0 until 32) {
      // 8 bits of each tile
      for (j <- 0 until 8) {
        val pixel = frameBuffer(scanLine * 256 + (i * 8) + j)
        if (pixel.value != 0) {
          // do nothing
        } else {
          // current what?
          val curr = 15 - j - rFineX
          val pixelIndex = (rLowShift >> curr) & 0x01 | ((rHighShift >> curr) & 0x01) << 1
          val palette =
            if (curr >= 8) readBackgroundPalette(attr, pixelIndex) else readBackgroundPalette(attrTmp, pixelIndex)
          pixel.color = Colors.table(palette % 64)
          pixel.value = pixelIndex
          pixel.pIndex = -1
        }
      }

      attr = attrTmp

      readBackgroundTile() match {
        case (a, b, c) => low = a; high = b; attrTmp = c
      }
      rLowShift = ((rLowShift << 8) | low).toShort
      rHighShift = ((rHighShift << 8) | high).toShort
    }
  }

  def renderTile(tile: Array[Byte],
                 yOffset: Int,
                 spr: SpriteObject,
                 index: Int): Unit = {
    val isSpr0 = index == 0
    val y = spr.y + yOffset

    for (i <- 0 until 8) {
      breakable {
        var xCoord = if (spr.horizontalFlip()) spr.x + i else spr.x + 7 - i

        if (xCoord <= 255) {
          val row = y * 256 + xCoord
          // bit 0 & 1
          var pixel = (tile(0) >> i) & 0x01
          pixel += ((tile(1) >> i) & 0x01) << 1
          val isTransparent = spr.attribute == 0 && pixel == 0

          if (row < 0xf000 && !isTransparent) {
            val px = frameBuffer(row)
            if (px.value != 0 && isSpr0 && rStatus.sprite0Hit()) {
              // FIXME ?
              rStatus.setSprite0Hit(true)
            }

            if (px.pIndex > -1 && px.pIndex < index) {
              // px.priority > current pixel,  do nothing
              break

            } else if (px.value != 0 && spr.backgroundPrior()) {
              // do nothing
              break
            }

            val paletteIndex = palettes(0x10 + spr.upperColor2Bits() * 0x4 + pixel)
            px.color = Colors.table(paletteIndex)
            px.value = pixel
            px.pIndex = index
          }
        }
      }
    }
  }

  def updateEndScanLine(): Unit = {
    if ((rAddress & 0x1f) == 0x1f) {
      rAddress ^= 0x41f
    } else {
      rAddress += 1
    }

    // TODO need twice?
    if (rMask.background() || rMask.sprite()) {
      if ((rAddress & 0x7000) == 0x7000) {
        val tmp = rAddress & 0x3e0
        rAddress &= 0xfff

        tmp match {
          case 0x3a0 => rAddress ^= 0xba0
          case 0x3e0 => rAddress ^= 0x3e0
          case _ => rAddress += 0x20
        }
      } else {
        rAddress += 0x1000
      }
      rAddress = (rAddress & 0x7be0) | (rLatchTemp & 0x41f)
    }
  }

  /**
   * Evaluate a scanLine.
   *
   * @param line
   */
  def evalSprites(line: Int): Unit = {
    var sprCount = 0
    val sprHeight = if (rControl.spriteSize()) 16 else 8

    var i = 0
    for (spr <- oam.sprites) {
      val y = IntUtils.toUnsigned(spr.y)
      //  line >= y && line <= y + sprHeight
      if (line >= y && line <= (y + sprHeight)) {
        val upper2ColorBits = spr.upperColor2Bits()
        val tileIndex = spr.index
        val c = line - y - 1
        var yCoord = if (spr.verticalFlip()) y + sprHeight - 1 - c else y + c + 1

        if (rControl.spriteSize()) { // 8 * 16
          val addr = spritePatternAddress(tileIndex)
          val upperPart = M.rom.readTile(addr)
          val lowerPart = M.rom.readTile(addr + TileSize)
          var tile: Array[Byte] = null
          if (c > 7 && spr.verticalFlip()) {
            tile = upperPart
            yCoord += 8
          } else if (c < 8 && spr.verticalFlip()) {
            tile = lowerPart
            yCoord -= 8
          } else if (c > 7)
            tile = lowerPart
          else
            tile = upperPart

          val isSpr0 = i == 0
          renderTile( Array(tile(c % 8), tile(c % 8 + 8)), yCoord - y, spr, i)
        } else {
          // 8 * 8
          val addr = spritePatternAddress(tileIndex)
          val tile = M.rom.readTile(addr)
          renderTile(Array(tile(c), tile(c+8)), yCoord - y, spr, i)
        }

        sprCount += 1
        // TODO enabled sprite limited
        if (sprCount > 8) {
          rStatus.setSpriteOverflow(true)
          return
        }
      }
      i += 1
    }
  }

  /**
   * 渲染至界面
   */
  def raster(): Unit = {
    // Set a condition variable.
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

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
class Ppu {

  // cpu use these registers to read/write ppu
  // 0x2000
  val control = new ControlRegister

  // 0x2001
  val mask = new MaskRegister

  // 0x2002
  val status = new StatusRegister

  // 0x2003
  var oamAddr: Byte = 0

  // 0x2004
  var oamData: Byte = 0

  // 0x2005, Write twice
  var scroll: Byte = 0

  var scrollLatch: Boolean = false

  // 0x2006, Write twice
  var address: Byte = 0

  var addressLatch: Boolean = false

  // 0x2007
  var data: Byte = 0

  // 0x4014
  var oamDma: Byte = 0

  var vram = new VideoRam

  var currentScanline = 0

  def runStep(): Unit = {

  }
}

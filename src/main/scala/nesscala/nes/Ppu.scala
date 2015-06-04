package nesscala.nes

import nesscala.nes.ppu._

/**
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
}

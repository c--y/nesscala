package nesscala.nes.ppu

/**
 * Created by chenyan on 15-6-6.
 */
class Palettes {

  // 0x3f00 ~ 0x10, PlayField palette
  // 0x3f10 ~ 0x10, Sprite Palette
  val store = Array.fill(2)(new Array[Byte](0x10))

}

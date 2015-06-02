package nesscala.nes.ppu

/**
 * SPR-RAM
 *
 *  64 sprites, 256 bytes area of memory, independent of ROM and VRam.
 *
 *  Sprite data is kept within the Pattern table region of VRam.
 *
 *  Sprite attributes such as flipping and priority, are stored in SPR-RAM
 *
 * Created by chenyan on 15-6-3.
 */
class SpriteRam {

  // 256bytes
  var units: Array[Byte] = null

  val sprites = new Array[SpriteAttribute](64)

  def load(bytes: Array[Byte]): Unit = {
    require(bytes.length == 0x100)

    for (i <- 0 until 64) {
      val j = i * 4
      sprites(i) = new SpriteAttribute(
        bytes(j), bytes(j + 1), bytes(j + 2), bytes(j + 3))
    }

    units = bytes
  }
}

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
class SpriteRam(val size: Int) {

  // 4 bytes
  val SpriteSize = 4

  // 256 bytes
  var stores = new Array[Byte](SpriteSize * size)

  // 64 sprites
  val sprites = Array.fill(size) {new SpriteObject}

  def get(index: Int): SpriteObject =
    sprites(index)

  def read(index: Int): Byte =
    stores(index)

  def write(index: Int, value: Byte): Unit = {
    require(index >= 0 && index < size * SpriteSize)
    
    stores(index) = value

    val i = index / SpriteSize
    val m = index % SpriteSize

    m match {
      case 0x0 => sprites(i).y = value
      case 0x1 => sprites(i).index = value
      case 0x2 => sprites(i).attribute = value
      case 0x3 => sprites(i).x = value
    }
  }
}

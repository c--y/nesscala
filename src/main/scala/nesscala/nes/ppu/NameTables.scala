package nesscala.nes.ppu

/**
 * Created by chenyan on 15-5-31.
 */
class NameTables {

  // Two actual nametables in vram
  //  0: 0x2000, 0x400 bytes
  //  1: 0x2400, 0x400 bytes
  val store = Array.fill(2){new Array[Byte](0x1000)}


}

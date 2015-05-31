package nesscala.nes

import nesscala.nes.ppu.NameTables

/**
 * Created by chenyan on 15-5-30.
 */
class Ppu {

  // 16kb video ram
  val vram: Array[Byte] = new Array(16 * 1024)

  // 4 nametables
  val nameTables = new NameTables
}

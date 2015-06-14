package nesscala.nes.ppu

/**
 * NameTable
 *
 *
 * Created by chenyan on 15-5-31.
 */
class NameTables {

  // 64 bytes per attribute table
  val AttributeTableSize = 0x40

  // Two actual nametables in vram
  //  0: 0x2000, 0x400 bytes
  //  1: 0x2400, 0x400 bytes
  val store = Array.fill(2){new Array[Byte](0x400)}

  // Logic tables
  val tables = new Array[Array[Byte]](4)

  def mirror(t: Symbol): Unit = t match {
    case 'Horizontal => {
      tables(0) = store(0)
      tables(1) = store(0)
      tables(2) = store(1)
      tables(3) = store(1)
    }
    case 'Vertical => {
      tables(0) = store(0)
      tables(1) = store(1)
      tables(2) = store(0)
      tables(3) = store(1)
    }
    case 'SingleScreen0 => {
      tables(0) = store(0)
      tables(1) = store(0)
      tables(2) = store(0)
      tables(3) = store(0)
    }
    case 'SingleScreen1 => {
      tables(0) = store(1)
      tables(1) = store(1)
      tables(2) = store(1)
      tables(3) = store(1)
    }

    case _ => throw new RuntimeException
  }

  def read(address: Int): Int =
    tables((address & 0xc00) >> 10)(address & 0x3ff)

  def write(address: Int, value: Byte): Unit =
    tables((address & 0xc00) >> 10)(address & 0x3ff) = value

  /**
   * 读取vram address对应的attribute
   *
   * @param address 指向nametable的地址
   * @return
   */
  def readAttr(address: Int): Int = {
    // 32 * 32 tiles matrix index (NameTable index)
    val i = address & 0x3ff
    // (i / 32) / 8
    val r = i >> 8
    // (i % 32) / 8
    val c = (i % 32) >> 3
    tables((address & 0xc00) >> 10)(r << 3 + c)
  }

  /**
   * 解析attribute字节.
   *
   * 返回值结构是
   *
   * 7 6 5 4 3 2 1 0
   * (7, 6) = upper 2 bits for square3
   * (5, 4) = upper 2 bits for square2
   * (3, 2) = upper 2 bits for square1
   * (1, 0) = upper 2 bits for square0
   *
   * @param b
   * @return (10, 32, 54, 76)
   */
  def parseAttrByte(b: Byte): (Int, Int, Int, Int) =
    (b & 0x3, (b & 0xc) >> 2, (b & 0x30) >> 4, (b & 0xc0) >> 6)
}

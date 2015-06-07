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
}

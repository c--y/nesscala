package nesscala.nes

import nesscala.util.IntUtils

/**
 * Created by chenyan on 15-5-31.
 */
class Memory(val size: Int) {

  val ram: Array[Byte] = new Array(size)

  def read(address: Int): Int =
    IntUtils.toUnsigned(ram(address))

  def write(address: Int, v: Byte) =
    ram(address) = v

}

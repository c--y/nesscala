package nesscala.nes

import nesscala.util.IntUtils

/**
 * Created by chenyan on 15-5-31.
 */
class Memory(val size: Int) {

  val ram: Array[Byte] = new Array(size)

  def read(address: Int): Int =
    // TODO 是否应该做这个处理
    //IntUtils.toUnsigned(ram(address))
    ram(address)

  def write(address: Int, v: Byte) =
    ram(address) = v
}

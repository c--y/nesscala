package nesscala.nes

/**
 * Created by chenyan on 15-5-31.
 */
class Memory(val size: Int) {

  val ram: Array[Byte] = new Array(size)

  def read(address: Int): Byte =
    ram(address)

  def write(address: Int, v: Byte) =
    ram(address) = v

}

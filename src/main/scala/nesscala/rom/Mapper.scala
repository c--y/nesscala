package nesscala.rom

/**
 * Mapper接口
 *
 * Created by chenyan on 15-5-30.
 */
trait Mapper {

  def read(address: Int): Byte

  def write(address: Int, value: Byte): Unit

  def step(): Unit

  def readVram(address: Int): Byte

  def writeVram(address: Int, value: Byte): Unit

}

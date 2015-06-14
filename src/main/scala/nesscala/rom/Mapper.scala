package nesscala.rom

/**
 * Mapper接口
 *
 * 为计算方便,所有read*操作, 读取的byte, 全部提升至int类型
 *
 * Created by chenyan on 15-5-30.
 */
trait Mapper {

  def read(address: Int): Int

  def write(address: Int, value: Byte): Unit

  def readTile(address: Int): Array[Byte]

  def readVram(address: Int): Int

  def writeVram(address: Int, value: Byte): Unit

  def getType(): Symbol

}

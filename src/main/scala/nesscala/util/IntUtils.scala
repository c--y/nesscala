package nesscala.util

/**
 * Created by chenyan on 15-5-31.
 */
object IntUtils {

  def toUnsigned(i: Short): Int =
    i & 0x0000ffff

  def toUnsigned(b: Byte): Int =
    b & 0x000000ff

}

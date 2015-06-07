package nesscala.util

/**
 * Created by chenyan on 15-5-31.
 */
object IntUtils {

  def toUnsigned(i: Short): Int =
    i & 0xffff

  def toUnsigned(b: Byte): Int =
    b & 0xff

}

package nesscala.util

/**
 * Created by chenyan on 15-5-31.
 */
object IntUtils {

  def toUnsigned(i: Short): Int =
    if (i < 0) Short.MaxValue - i else i

  def toUnsigned(b: Byte): Int =
    if (b < 0) Byte.MaxValue - b else b

}

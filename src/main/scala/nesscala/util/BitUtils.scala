package nesscala.util

/**
  * Created by chenyan on 15-5-30.
  */
object BitUtils {

  def makeUint8(low: Byte, high: Byte): Byte =
    (IntUtils.toUnsigned(low) | (IntUtils.toUnsigned(high) << 4)).toByte

  def makeWord(low: Byte, high: Byte): Int =
    IntUtils.toUnsigned(low) | (IntUtils.toUnsigned(high) << 8)

  def byteMask(nth: Byte): Int =
    nth match {
      case 0 => 0x1
      case 1 => 0x2
      case 2 => 0x4
      case 3 => 0x8
      case 4 => 0x10
      case 5 => 0x20
      case 6 => 0x40
      case 7 => 0x80
      case _ => throw new RuntimeException
    }

  def isSet(byte: Byte, nth: Byte): Boolean =
    (byte & byteMask(nth)) > 0

  def setBit(byte: Byte, nth: Byte, v: Boolean): Byte =
    if (v) (byte | byteMask(nth)).toByte else (byte ^ byteMask(nth)).toByte


}

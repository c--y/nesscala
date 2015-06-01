package nesscala.nes.cpu

import nesscala.util.BitUtils

/**
 * C - Carry flag
 * Z - Zero flag
 * I - Interrupt enabled
 * D - Decimal
 * B - BRK
 * V - Overflow flag
 * S - Sign flag
 *
 * 7 6 5 4 3 2 1 0
 * S V   B D I Z C
 *
 * Created by chenyan on 15-5-31.
 */
class StatusRegister {

  var v: Byte = 0

  def carry(): Boolean =
    BitUtils.isSet(v, 0)

  def zero(): Boolean =
    BitUtils.isSet(v, 1)

  def interrupt(): Boolean =
    BitUtils.isSet(v, 2)

  def decimal(): Boolean =
    BitUtils.isSet(v, 3)

  def brk(): Boolean =
    BitUtils.isSet(v, 4)

  def overflow(): Boolean =
    BitUtils.isSet(v, 6)

  def sign(): Boolean =
    BitUtils.isSet(v, 7)

  def setCarry(value: Boolean) =
    v = BitUtils.setBit(v, 0, value)

  def setZero(value: Boolean) =
    v = BitUtils.setBit(v, 1, value)

  def setInterrupt(value: Boolean) =
    v = BitUtils.setBit(v, 2, value)

  def setDecimal(value: Boolean) =
    v = BitUtils.setBit(v, 3, value)

  def setBrk(value: Boolean) =
    v = BitUtils.setBit(v, 4, value)

  def setOverflow(value: Boolean) =
    v = BitUtils.setBit(v, 6, value)

  def setSign(value: Boolean) =
    v = BitUtils.setBit(v, 7, value)

  def testAndSetNegative(value: Byte) =
    setSign((value & 0x80) == 0x80)

  def testAndSetZero(value: Byte) =
    setZero(value == 0x00)

  def testAndSetCarryPlus(value: Int) =
    setCarry(value > 0xff)

  def testAndSetCarryMinus(value: Int) =
    setCarry(value >= 0x00)

  /**
   * a + b = c
   *
   * a与b符号相同, 且a与c符号不同, 则表明c值溢出了
   *
   * @param a
   * @param b
   * @param c
   */
  def testAndSetOverflowPlus(a: Int, b: Int, c: Int) =
    setOverflow((((a ^ b) & 0x80) == 0x00) && (((a ^ c) & 0x80) == 0x80))

  /**
   * a - b - carry = c
   *
   * @param a
   * @param b
   */
  def testAndSetOverflowMinus(a: Int, b: Int) = {
    val result = a - b - (1 - (if (carry()) 1 else 0))
    setOverflow(((a ^ result) & 0x80) != 0 && ((a ^ b) & 0x80) != 0)
  }
}

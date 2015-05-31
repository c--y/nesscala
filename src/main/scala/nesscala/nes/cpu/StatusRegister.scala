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

}

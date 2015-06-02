package nesscala.nes

import nesscala.nes.cpu.StatusRegister
import nesscala.util.{IntUtils, BitUtils}

/**
 * 6502 CPU
 *
 * Created by chenyan on 15-5-30.
 */
class Cpu (val ram: Memory) {

  // Registers
  // pc: Short
  var pc: Int = 0

  var sp: Byte = 0

  var acc: Byte = 0

  var x: Byte = 0

  var y: Byte = 0

  val p: StatusRegister = new StatusRegister

  // Environments
  var cycles: Long = 0

  var opcode: Byte = 0

  var opcodeCycle: Long = 0

  def pushStack(v: Byte): Unit = {
    ram.write((0x100 + sp).toShort, v)
    sp = (sp - 1).toByte
  }

  def pullStack(): Int = {
    sp = (sp + 1).toByte
    ram.read((0x100 + sp).toShort)
  }

  // Address modes
  def samePage(a1: Int, a2: Int): Boolean =
    (a1 & 0xff00) == (a2 & 0xff00)

  def immediateAddress(): Int = {
    pc += 1
    pc - 1
  }

  def absoluteAddress(): Int = {
    val high = ram.read(pc + 1)
    val low = ram.read(pc + 1)
    pc += 2
    BitUtils.makeWord(low.toByte, high.toByte)
  }

  def zeroPageAddress(): Int = {
    pc += 1
    ram.read(pc - 1)
  }

  def indirectAddress(): Int = {
    return indirectAbsoluteAddress(pc)
  }

  def indirectAbsoluteAddress(address: Int): Int = {
    val high = ram.read(address + 1)
    val low = ram.read(address)

    val highAddr = BitUtils.makeWord(low.toByte, high.toByte)
    val lowAddr = BitUtils.makeWord((low + 1).toByte, high.toByte)

    val hhigh = ram.read(highAddr)
    val llow = ram.read(lowAddr)

    BitUtils.makeUint8(llow.toByte, hhigh.toByte)
  }

  def absoluteIndexedAddress(index: Byte): Int = {
    val high = ram.read(pc + 1)
    val low = ram.read(pc)

    //val uindex = IntUtils.toUnsigned(index)
    var address = BitUtils.makeWord(low.toByte, high.toByte)
    // 不同页罚项
    cycles += (if (!samePage(address, address + index)) 1 else 0)

    address += index
    pc += 2
    if (address > 0xffff) address & 0xffff else address
  }

  def zeroPageIndexedAddress(index: Byte): Int = {
    val address = ram.read(pc)
    pc += 1
    address + index
  }

  def indexedIndirectAddress(): Int = {
    val address = ram.read(pc) + x
    pc += 1
    val high = ram.read(address + 1)
    val low = ram.read(address)
    BitUtils.makeWord(low.toByte, high.toByte)
  }

  def indirectIndexedAddress(): Int = {
    val address = ram.read(pc)
    val high = ram.read(address + 1)
    val low = ram.read(address)

    var indAddress = BitUtils.makeWord(low.toByte, high.toByte)
    // 位于不同页
    cycles += (if (!samePage(indAddress, indAddress + y)) 1 else 0)

    indAddress += y
    pc += 1
    if (indAddress > 0xffff) indAddress & 0xffff else indAddress
  }

  def relativeAddress(): Int = {
    var address = ram.read(pc)
    address = if (address < 0x80) address + pc else address + (pc - 0x100)
    address + 1
  }

  def accumulatorAddress(): Int = 0

  def penaliseBranchCycles(address: Int): Unit = {
    // 判定pc 与 address是否为同一页, 如果是则多加1个时钟周期
    cycles += (if (!samePage(pc - 1, address)) 2 else 1)
  }

}

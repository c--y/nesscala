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

  var sp: Int = 0

  var acc: Byte = 0

  var x: Byte = 0

  var y: Byte = 0

  val p: StatusRegister = new StatusRegister

  // Environments
  var cycles: Long = 0

  var opcode: Byte = 0

  def pushStack(v: Byte): Unit = {
    ram.write((0x100 + sp).toShort, v)
    sp = (sp - 1).toByte
  }

  def pullStack(): Byte = {
    sp = (sp + 1).toByte
    ram.read((0x100 + sp).toShort)
  }

  // Address modes
  def immediateAddress(): Int = {
    pc += 1
    pc - 1
  }

  def absoluteAddress(): Int = {
    val high = ram.read(pc + 1)
    val low = ram.read(pc + 1)
    pc += 2
    BitUtils.makeWord(low, high)
  }

  def zeroPageAddress(): Int = {
    pc += 1
    ram.read(pc - 1)
  }

  def indirectAbsoluteAddress(address: Int): Int = {
    val high = ram.read(address + 1)
    val low = ram.read(address)

    val highAddr = BitUtils.makeWord(low, high)
    val lowAddr = BitUtils.makeWord((IntUtils.toUnsigned(low) + 1).toByte, high)

    val hhigh = ram.read(highAddr)
    val llow = ram.read(lowAddr)

    BitUtils.makeUint8(llow, hhigh)
  }

  def absoluteIndexedAddress(index: Byte): Int = {
    val high = ram.read(pc + 1)
    val low = ram.read(pc)

    val uindex = IntUtils.toUnsigned(index)
    var address = BitUtils.makeWord(low, high)
    if ((address & 0xff00) != ((address + uindex) & 0xff00)) {
      cycles += 1
    }

    address += uindex
    pc += 2
    if (address > 0xffff) address & 0xffff else address
  }

  def zeroPageIndexedAddress(index: Byte): Int = {
    val uindex = IntUtils.toUnsigned(index)

    val address = ram.read(pc)
    pc += 1
    IntUtils.toUnsigned(address) + index
  }

  def indexedIndirectAddress(): Int = {
    val address = IntUtils.toUnsigned(ram.read(pc)) + IntUtils.toUnsigned(x)
    pc += 1
    val high = ram.read(address + 1)
    val low = ram.read(address)
    BitUtils.makeWord(low, high)
  }

  def indirectIndexedAddress(): Int = {
    val address = IntUtils.toUnsigned(ram.read(pc))
    val high = ram.read(address + 1)
    val low = ram.read(address)

    var indAddress = BitUtils.makeWord(low, high)
    val uy = IntUtils.toUnsigned(this.y)
    if ((indAddress & 0xff00) != ((indAddress + uy) & 0xff00))) {
      cycles += 1
    }

    indAddress += uy
    pc += 1
    if (indAddress > 0xffff) indAddress & 0xffff else indAddress
  }

  def relativeAddress(): Int = {
    var address = IntUtils.toUnsigned(ram.read(pc))
    address = if (address < 0x80) address + pc else address + (pc - 0x100)
    address + 1
  }

  def accumulatorAddress(): Int = 0



}

package nesscala.nes

import nesscala.nes.cpu._
import nesscala.rom.{Disassembler, Mapper}
import nesscala.util.{IntUtils, BitUtils}

/**
 * 6502 CPU
 *
 * Created by chenyan on 15-5-30.
 */
class Cpu () {

  val executor = new Executor(this)

  // Registers
  // pc: Short
  var pc: Int = 0

  var sp: Byte = 0xfd.toByte

  var acc: Byte = 0

  var x: Byte = 0

  var y: Byte = 0

  val p = new StatusRegister

  // Environments
  var cycle: Long = 0

  var totalCycles: Long = 0

  var opcode: Byte = 0

  var interrupt: Interrupt = InterruptNone

  def pushStack(v: Byte): Unit = {
    val _sp = IntUtils.toUnsigned(sp)
    M.ram.write((0x100 + _sp).toShort, v)
    sp = (_sp - 1).toByte
  }

  def pullStack(): Int = {
    val _sp = IntUtils.toUnsigned(sp)
    sp = (_sp + 1).toByte
    M.ram.read((0x100 + _sp + 1).toShort)
  }

  // Address modes
  def samePage(a1: Int, a2: Int): Boolean =
    (a1 & 0xff00) == (a2 & 0xff00)

  def immediateAddress(): Int = {
    pc += 1
    pc - 1
  }

  def absoluteAddress(): Int = {
    val high = M.ram.read(pc + 1)
    val low = M.ram.read(pc)
    pc += 2
    BitUtils.makeWord(low.toByte, high.toByte)
  }

  def zeroPageAddress(): Int = {
    pc += 1
    M.ram.read(pc - 1)
  }

  def indirectAddress(): Int = {
    return indirectAbsoluteAddress(pc)
  }

  def indirectAbsoluteAddress(address: Int): Int = {
    val high = M.ram.read(address + 1)
    val low = M.ram.read(address)

    val highAddr = BitUtils.makeWord(low.toByte, high.toByte)
    val lowAddr = BitUtils.makeWord((low + 1).toByte, high.toByte)

    val hhigh = M.ram.read(highAddr)
    val llow = M.ram.read(lowAddr)

    BitUtils.makeUint8(llow.toByte, hhigh.toByte)
  }

  def absoluteIndexedAddress(index: Byte): Int = {
    val high = M.ram.read(pc + 1)
    val low = M.ram.read(pc)

    //val uindex = IntUtils.toUnsigned(index)
    var address = BitUtils.makeWord(low.toByte, high.toByte)
    // 不同页罚项
    cycle += (if (!samePage(address, address + index)) 1 else 0)

    address += index
    pc += 2
    if (address > 0xffff) address & 0xffff else address
  }

  def zeroPageIndexedAddress(index: Byte): Int = {
    val address = M.ram.read(pc)
    pc += 1
    address + index
  }

  def indexedIndirectAddress(): Int = {
    val address = M.ram.read(pc) + x
    pc += 1
    val high = M.ram.read(address + 1)
    val low = M.ram.read(address)
    BitUtils.makeWord(low.toByte, high.toByte)
  }

  def indirectIndexedAddress(): Int = {
    val address = M.ram.read(pc)
    val high = M.ram.read(address + 1)
    val low = M.ram.read(address)

    var indAddress = BitUtils.makeWord(low.toByte, high.toByte)
    // 位于不同页
    cycle += (if (!samePage(indAddress, indAddress + y)) 1 else 0)

    indAddress += y
    pc += 1
    if (indAddress > 0xffff) indAddress & 0xffff else indAddress
  }

  def relativeAddress(): Int = {
    var address = M.ram.read(pc)
    address = if (address < 0x80) address + pc else address + (pc - 0x100)
    address + 1
  }

  def accumulatorAddress(): Int = 0

  def penaliseBranchCycles(address: Int): Unit = {
    // 判定pc 与 address是否为同一页, 如果是则多加1个时钟周期
    cycle += (if (!samePage(pc - 1, address)) 2 else 1)
  }

  def runStep(): Long = {
    if (cycle > 0) {
      cycle -= 1
      return 1
    }

    interrupt match {
      case InterruptIrq =>
        // TODO mmc5
        if (M.rom.getType() == 'mmc5) {

        } else if (!p.interrupt()) {
          irq()
          interrupt = InterruptNone
        }
      case InterruptNmi =>
        nmi()
        interrupt = InterruptNone
      case InterruptReset =>
        reset()
        interrupt = InterruptNone
      case InterruptNone =>
        {}
    }

    val c = M.ram.read(pc)
    println("%s %s".format(Disassembler.dis(c, pc, this), this))
    pc += 1
    executor.fnTable(c)()
    totalCycles += cycle
    cycle
  }

  def irq(): Unit = {
    val high = pc >> 8
    val low = pc & 0xff
    pushStack(high.toByte)
    pushStack(low.toByte)
    pushStack(p.v)
    pc = BitUtils.makeWord(M.ram.read(0xfffe).toByte, M.ram.read(0xffff).toByte)
  }

  def nmi(): Unit = {
    val high = pc >> 8
    val low = pc & 0xff
    pushStack(high.toByte)
    pushStack(low.toByte)
    pushStack(p.v)

    pc = BitUtils.makeWord(M.ram.read(0xfffa).toByte, M.ram.read(0xfffb).toByte)
  }

  def reset(): Unit =
    pc = BitUtils.makeWord(M.ram.read(0xfffc).toByte, M.ram.read(0xfffd).toByte)

  def requestInterrupt(r: Interrupt): Unit =
    interrupt = r

  override def toString(): String =
    "A:%02X X:%02X Y:%02X P:%02X SP:%02X CYC:%3d SL:%d".format(
      IntUtils.toUnsigned(acc),
      IntUtils.toUnsigned(x),
      IntUtils.toUnsigned(y),
      IntUtils.toUnsigned(p.v),
      IntUtils.toUnsigned(sp),
      totalCycles,
      M.ppu.scanLine
    )
}

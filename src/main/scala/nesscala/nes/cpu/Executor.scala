package nesscala.nes.cpu

import nesscala.nes.Cpu
import nesscala.util.IntUtils

/**
 * Opcode executor
 *
 * Created by chenyan on 15-6-1.
 */
class Executor(cpu: Cpu) {

  val fnTable: Array[Function0[Unit]] = new Array(0xff)

  def defineOp(fn: Any,
               cycles: Long,
               addressFn: Any,
               addressParam: Int): Function0[Unit] =
    () => {
      cpu.cycles = cycles

      if (addressFn != null) { // 有地址参的opcode
        val address =
          if (addressParam != null)
            addressFn.asInstanceOf[Function1[Int, Int]].apply(addressParam)
          else
            addressFn.asInstanceOf[Function0[Int]].apply()

        fn.asInstanceOf[Function1[Int, Unit]](address)
      } else {  // 无地址参的opcode
        fn.asInstanceOf[Function0[Unit]]()
      }
    }

  // Initialize the function table

  def ADC(address: Int): Unit = {
    val v = cpu.ram.read(address)
    val carry = if (cpu.p.carry()) 1 else 0
    val result = IntUtils.toUnsigned(cpu.acc) + v + carry
    val old = cpu.acc
    cpu.acc = result.toByte

    cpu.p.testAndSetNegative(cpu.acc)
    cpu.p.testAndSetZero(cpu.acc)
    cpu.p.testAndSetCarryPlus(result)
    cpu.p.testAndSetOverflowPlus(old, v, IntUtils.toUnsigned(cpu.acc))
  }

  def AND(address: Int): Unit = {
    val v = cpu.ram.read(address)
    cpu.acc = (cpu.acc & v).toByte

    cpu.p.testAndSetNegative(cpu.acc)
    cpu.p.testAndSetZero(cpu.acc)
  }

  def ASL(address: Int): Unit = {
    val v = cpu.ram.read(address)
    cpu.p.setCarry((v & 0x80) > 0)

    val shiftV = (v << 1).toByte
    cpu.ram.write(address, shiftV)

    cpu.p.testAndSetNegative(shiftV)
    cpu.p.testAndSetZero(shiftV)
  }

  def ASL_acc(): Unit = {
    cpu.p.setCarry((cpu.acc & 0x80) > 0)
    cpu.acc = (cpu.acc << 1).toByte

    cpu.p.testAndSetNegative(cpu.acc)
    cpu.p.testAndSetZero(cpu.acc)
  }

  def BCC(): Unit = {
    if (cpu.p.carry()) {
      val address = cpu.relativeAddress()
      cpu.pc = address
      cpu.penaliseBranchCycles(address)
    }
  }

  /**
   * Branch if P.C is set.
   */
  def BCS(address: Int): Unit = {
    val v = cpu.ram.read(address)
    if (cpu.p.carry()) {
      cpu.pc += v
      cpu.penaliseBranchCycles(address)
    }
  }

  /**
   * Branch if P.Z is set
   *
   * @param address
   */
  def BEQ(address: Int): Unit = {

  }

}

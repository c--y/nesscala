package nesscala.nes.cpu

import nesscala.nes.Cpu
import nesscala.util.{BitUtils, IntUtils}

/**
 * Opcode executor
 *
 * Created by chenyan on 15-6-1.
 */
class Executor(cpu: Cpu) {

  val NoneAddressParam: Int = -1

  val fnTable: Array[Function0[Unit]] = new Array(0xff)

  def defineOp(fn: Any,
               cycles: Long,
               addressFn: Any,
               addressParam : => Int): Function0[Unit] =
    () => {
      cpu.cycles = cycles

      if (addressFn != null) { // 有地址参的opcode
        val address =
          // 负值代表没有参数
          if (addressParam >= 0)
            addressFn.asInstanceOf[Function1[Int, Int]].apply(addressParam)
          else
            addressFn.asInstanceOf[Function0[Int]].apply()

        fn.asInstanceOf[Function1[Int, Unit]](address)
      } else {  // 无地址参的opcode
        fn.asInstanceOf[Function0[Unit]]()
      }
    }

  // Initialize the function table
  // Implied
  fnTable(0x00) = defineOp(BRK, 7, null, -1)
  fnTable(0x01) = defineOp(ORA _, 6, cpu.indirectIndexedAddress, -1)
  fnTable(0x05) = defineOp(ORA _, 2, cpu.zeroPageAddress, -1)
  fnTable(0x06) = defineOp(ASL _, 5, cpu.zeroPageAddress, -1)
  // Implied
  fnTable(0x08) = defineOp(PHP, 3, null, -1)
  fnTable(0x09) = defineOp(ORA _, 2, cpu.immediateAddress, -1)
  // Accumulator
  fnTable(0x0a) = defineOp(ASL_acc, 2, null, -1)
  fnTable(0x0d) = defineOp(ORA _, 4, cpu.absoluteAddress, -1)
  fnTable(0x0e) = defineOp(ASL _, 6, cpu.absoluteAddress, -1)
  // Implied
  fnTable(0x10) = defineOp(BPL _, 2, null, -1)
  fnTable(0x11) = defineOp(ORA _, 5, cpu.indirectIndexedAddress, -1)
  fnTable(0x15) = defineOp(ORA _, 3, cpu.zeroPageIndexedAddress _, cpu.x)
  fnTable(0x16) = defineOp(ASL _, 6, cpu.zeroPageIndexedAddress _, cpu.x)
  fnTable(0x18) = defineOp(CLC, 2, null, -1)
  fnTable(0x19) = defineOp(ORA _, 4, cpu.absoluteIndexedAddress _, cpu.y)
  fnTable(0x1d) = defineOp(ORA _, 4, cpu.absoluteIndexedAddress _, cpu.x)
  fnTable(0x1e) = defineOp(ASL _, 7, cpu.absoluteIndexedAddress _, cpu.x)
  fnTable(0x20) = defineOp(JSR _, 6, cpu.absoluteAddress, -1)
  fnTable(0x21) = defineOp(AND _, 6, cpu.indirectIndexedAddress, -1)
  fnTable(0x24) = defineOp(BIT _, 3, cpu.zeroPageAddress, -1)
  fnTable(0x25) = defineOp(AND _, 2, cpu.zeroPageAddress, -1)
  fnTable(0x26) = defineOp(ROL _, 5, cpu.zeroPageAddress, -1)
  // Implied
  fnTable(0x28) = defineOp(PLP, 4, null, -1)
  fnTable(0x29) = defineOp(AND _, 2, cpu.immediateAddress, -1)
  // Accumulator
  fnTable(0x2a) = defineOp(ROL_acc, 2, null, -1)
  fnTable(0x2c) = defineOp(BIT _, 4, cpu.absoluteAddress, -1)
  fnTable(0x2d) = defineOp(AND _, 4, cpu.absoluteAddress, -1)
  fnTable(0x2e) = defineOp(ROL _, 6, cpu.absoluteAddress, -1)
  fnTable(0x30) = defineOp(BMI _, 2, cpu.relativeAddress, -1)
  fnTable(0x31) = defineOp(AND _, 5, cpu.indirectIndexedAddress, -1)
  fnTable(0x35) = defineOp(AND _, 3, cpu.zeroPageIndexedAddress _, cpu.x)
  fnTable(0x36) = defineOp(ROL _, 6, cpu.zeroPageIndexedAddress _, cpu.x)
  // Implied
  fnTable(0x38) = defineOp(SEC, 2, null, -1)
  fnTable(0x39) = defineOp(AND _, 4, cpu.absoluteIndexedAddress _, cpu.y)
  fnTable(0x3d) = defineOp(AND _, 4, cpu.absoluteIndexedAddress _, cpu.x)
  fnTable(0x3e) = defineOp(ROL _, 7, cpu.absoluteIndexedAddress _, cpu.x)
  // Implied
  fnTable(0x40) = defineOp(RTI, 4, null, -1)
  fnTable(0x41) = defineOp(EOR _, 6, cpu.indirectIndexedAddress, -1)
  fnTable(0x45) = defineOp(EOR _, 3, cpu.zeroPageAddress, -1)
  fnTable(0x46) = defineOp(LSR _, 5, cpu.zeroPageAddress, -1)
  // Implied
  fnTable(0x48) = defineOp(PHA, 3, null, -1)
  fnTable(0x49) = defineOp(EOR _, 2, cpu.immediateAddress, -1)
  // Accumulator
  fnTable(0x4a) = defineOp(LSR_acc, 2, null, -1)
  fnTable(0x4c) = defineOp(JMP _, 3, cpu.absoluteAddress, -1)
  fnTable(0x4d) = defineOp(EOR _, 4, cpu.absoluteAddress, -1)
  fnTable(0x4e) = defineOp(LSR _, 6, cpu.absoluteAddress, -1)
  fnTable(0x50) = defineOp(BVC _, 2, cpu.relativeAddress, -1)
  fnTable(0x51) = defineOp(EOR _, 5, cpu.indirectIndexedAddress, -1)
  fnTable(0x55) = defineOp(EOR _, 4, cpu.zeroPageIndexedAddress _, cpu.x)

  fnTable(0x56) = defineOp(LSR _, 6, cpu.zeroPageIndexedAddress _, cpu.x)
  // Implied
  fnTable(0x58) = defineOp(CLI, 2, null, -1)
  fnTable(0x59) = defineOp(EOR _, 4, cpu.absoluteIndexedAddress _, cpu.y)
  fnTable(0x5d) = defineOp(EOR _, 6, cpu.absoluteIndexedAddress _, cpu.x)
  fnTable(0x5e) = defineOp(LSR _, 7, cpu.absoluteIndexedAddress _, cpu.x)

  fnTable(0x60) = defineOp(RTS, 6, null, -1)
  fnTable(0x61) = defineOp(LSR _, 6, cpu.absoluteAddress, -1)
  fnTable(0x65) = defineOp(ADC _, 3, cpu.zeroPageAddress, -1)
  fnTable(0x66) = defineOp(ROR _, 5, cpu.zeroPageAddress, -1)
  fnTable(0x68) = defineOp(PLA, 6, null, -1)
  fnTable(0x69) = defineOp(ADC _, 2, cpu.immediateAddress, -1)
  // Accumulator
  fnTable(0x6a) = defineOp(ROR_acc, 2, null, -1)
  fnTable(0x6c) = defineOp(JMP _, 5, cpu.indirectAddress, -1)
  fnTable(0x6d) = defineOp(ADC _, 4, cpu.absoluteAddress, -1)
  fnTable(0x6e) = defineOp(ROR _, 6, cpu.absoluteAddress, -1)
  fnTable(0x70) = defineOp(BVS _, 2, cpu.relativeAddress(), -1)
  fnTable(0x71) = defineOp(ADC _, 5, cpu.absoluteAddress, -1)
  fnTable(0x75) = defineOp(ADC _, 6, cpu.zeroPageIndexedAddress _, cpu.x)
  fnTable(0x76) = defineOp(ROR _, 6, cpu.zeroPageIndexedAddress _, cpu.x)
  // Implied
  fnTable(0x78) = defineOp(SEI, 2, null, -1)
  fnTable(0x79) = defineOp(ADC _, 4, cpu.absoluteIndexedAddress _, cpu.y)
  fnTable(0x7d) = defineOp(ADC _, 4, cpu.absoluteIndexedAddress _, cpu.x)
  fnTable(0x7e) = defineOp(ROR _, 7, cpu.absoluteIndexedAddress _, cpu.x)
  fnTable(0x81) = defineOp(STA _, 6, cpu.indirectIndexedAddress, -1)
  fnTable(0x84) = defineOp(STY _, 3, cpu.zeroPageAddress, -1)
  fnTable(0x85) = defineOp(STA _, 3, cpu.zeroPageAddress, -1)
  fnTable(0x86) = defineOp(STX _, 6, cpu.zeroPageAddress, -1)
  // Implied
  fnTable(0x88) = defineOp(DEY, 2, null, -1)
  fnTable(0x8a) = defineOp(TXA, 2, null, -1)
  fnTable(0x8c) = defineOp(STY _, 4, cpu.absoluteAddress, -1)
  fnTable(0x8d) = defineOp(STA _, 4, cpu.absoluteAddress, -1)
  fnTable(0x8e) = defineOp(STX _, 4, cpu.absoluteAddress, -1)
  fnTable(0x90) = defineOp(BCC _, 2, cpu.relativeAddress(), -1)
  fnTable(0x91) = defineOp(STA _, 6, cpu.indirectIndexedAddress, -1)
  fnTable(0x94) = defineOp(STY _, 4, cpu.zeroPageIndexedAddress _, cpu.x)
  fnTable(0x95) = defineOp(STA _, 4, cpu.zeroPageIndexedAddress _, cpu.x)
  fnTable(0x96) = defineOp(STX _, 4, cpu.zeroPageIndexedAddress _, cpu.y)
  // Implied
  fnTable(0x98) = defineOp(TYA, 2, null, -1)
  fnTable(0x99) = defineOp(STA _, 5, cpu.absoluteIndexedAddress _, cpu.y)
  fnTable(0x9a) = defineOp(TXS, 2, null, -1)
  fnTable(0x9d) = defineOp(STA _, 6, cpu.absoluteIndexedAddress _, cpu.x)
  fnTable(0xa0) = defineOp(LDY _, 2, cpu.immediateAddress, -1)
  fnTable(0xa1) = defineOp(LDA _, 6, cpu.indirectIndexedAddress, -1)
  fnTable(0xa2) = defineOp(LDX _, 2, cpu.immediateAddress, -1)
  fnTable(0xa4) = defineOp(LDY _, 3, cpu.zeroPageAddress, -1)
  fnTable(0xa5) = defineOp(LDA _, 3, cpu.zeroPageAddress, -1)
  fnTable(0xa6) = defineOp(LDX _, 3, cpu.zeroPageAddress, -1)
  // Implied
  fnTable(0xa8) = defineOp(TAY, 2, null, -1)
  fnTable(0xa9) = defineOp(LDA _, 2, cpu.immediateAddress, -1)
  fnTable(0xaa) = defineOp(TAX, 2, null, -1)
  fnTable(0xac) = defineOp(LDY _, 4, cpu.absoluteAddress, -1)
  fnTable(0xad) = defineOp(LDA _, 4, cpu.absoluteAddress, -1)
  fnTable(0xae) = defineOp(LDX _, 4, cpu.absoluteAddress, -1)
  fnTable(0xb0) = defineOp(BCS _, 2, cpu.relativeAddress, -1)
  fnTable(0xb1) = defineOp(LDA _, 5, cpu.indirectIndexedAddress, -1)
  fnTable(0xb4) = defineOp(LDY _, 4, cpu.zeroPageIndexedAddress _, cpu.x)
  fnTable(0xb5) = defineOp(LDA _, 4, cpu.zeroPageIndexedAddress _, cpu.x)
  fnTable(0xb6) = defineOp(LDX _, 4, cpu.zeroPageIndexedAddress _, cpu.y)
  // Implied
  fnTable(0xb8) = defineOp(CLV, 2, null, -1)
  fnTable(0xb9) = defineOp(LDA _ _, 6, cpu.absoluteIndexedAddress _, cpu.y)
  // Implied
  fnTable(0xba) = defineOp(TSX, 2, null, -1)
  fnTable(0xbc) = defineOp(LDY _, 4, cpu.absoluteIndexedAddress _, cpu.x)
  fnTable(0xbd) = defineOp(LDA _, 6, cpu.absoluteIndexedAddress _, cpu.x)
  fnTable(0xbe) = defineOp(LDX _, 4, cpu.absoluteIndexedAddress _, cpu.y)
  fnTable(0xc0) = defineOp(CPY _, 2, cpu.immediateAddress, -1)
  fnTable(0xc1) = defineOp(CMP _, 2, cpu.indirectIndexedAddress, -1)
  fnTable(0xc4) = defineOp(CPY _, 3, cpu.zeroPageAddress, -1)
  fnTable(0xc5) = defineOp(CMP _, 3, cpu.zeroPageAddress, -1)
  fnTable(0xc6) = defineOp(DEC _, 5, cpu.zeroPageAddress, -1)
  // Implied
  fnTable(0xc8) = defineOp(INY, 2, null, -1)
  fnTable(0xc9) = defineOp(CMP _, 2, cpu.immediateAddress, -1)
  fnTable(0xca) = defineOp(DEC _, 2, null, -1)
  fnTable(0xcc) = defineOp(CPY _, 4, cpu.absoluteAddress, -1)
  fnTable(0xcd) = defineOp(CMP _, 4, cpu.absoluteAddress, -1)
  fnTable(0xce) = defineOp(DEC _, 6, cpu.absoluteAddress, -1)
  fnTable(0xd0) = defineOp(BNE _, 2, cpu.relativeAddress, -1)
  fnTable(0xd1) = defineOp(BNE _, 5, cpu.relativeAddress, -1)
  fnTable(0xd5) = defineOp(CMP _, 6, cpu.zeroPageIndexedAddress _, cpu.x)
  fnTable(0xd6) = defineOp(DEC _, 6, cpu.zeroPageIndexedAddress _, cpu.x)
  // Implied
  fnTable(0xd8) = defineOp(CLD, 2, null, -1)
  fnTable(0xd9) = defineOp(CMP _, 4, cpu.absoluteIndexedAddress _, cpu.y)
  fnTable(0xdd) = defineOp(CMP _, 4, cpu.absoluteIndexedAddress _, cpu.x)
  fnTable(0xde) = defineOp(DEC _, 7, cpu.absoluteIndexedAddress _, cpu.x)
  fnTable(0xe0) = defineOp(CPX _, 2, cpu.immediateAddress, -1)
  fnTable(0xe1) = defineOp(SBC _, 6, cpu.indirectIndexedAddress, -1)
  fnTable(0xe4) = defineOp(CPX _, 3, cpu.zeroPageAddress, -1)
  fnTable(0xe5) = defineOp(SBC _, 3, cpu.zeroPageAddress, -1)
  fnTable(0xe6) = defineOp(INC _, 5, cpu.zeroPageAddress, -1)
  // Implied
  fnTable(0xe8) = defineOp(INX, 2, null, -1)
  fnTable(0xe9) = defineOp(SBC _, 2, cpu.immediateAddress, -1)
  // Implied
  fnTable(0xea) = defineOp(NOP, 2, null, -1)
  fnTable(0xec) = defineOp(CPX _, 4, cpu.absoluteAddress, -1)
  fnTable(0xed) = defineOp(SBC _, 4, cpu.absoluteAddress, -1)
  fnTable(0xee) = defineOp(INC _, 6, cpu.absoluteAddress, -1)
  fnTable(0xf0) = defineOp(BEQ _, 2, cpu.relativeAddress, -1)
  fnTable(0xf1) = defineOp(SBC _, 5, cpu.indirectIndexedAddress, -1)
  fnTable(0xf5) = defineOp(SBC _, 4, cpu.zeroPageIndexedAddress _, cpu.x)
  fnTable(0xf6) = defineOp(INC _, 6, cpu.zeroPageIndexedAddress _, cpu.x)
  // Implied
  fnTable(0xf8) = defineOp(SED, 2, null, -1)
  fnTable(0xf9) = defineOp(SBC _, 4, cpu.absoluteIndexedAddress _, cpu.y)
  fnTable(0xfd) = defineOp(SBC _, 4, cpu.absoluteIndexedAddress _, cpu.x)
  fnTable(0xfe) = defineOp(INC _, 7, cpu.absoluteIndexedAddress _, cpu.x)

  // Basic operations
  def ADC(address: Int): Unit = {
    val v = cpu.ram.read(address)
    val carry = if (cpu.p.carry()) 1 else 0
    val result = cpu.acc + v + carry
    val old = cpu.acc
    cpu.acc = result.toByte

    cpu.p.testAndSetNegative(cpu.acc)
    cpu.p.testAndSetZero(cpu.acc)
    cpu.p.testAndSetCarryPlus(result)
    cpu.p.testAndSetOverflowPlus(old, v, cpu.acc)
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

  def BCC(address: Int): Unit = {
    if (cpu.p.carry()) {
      cpu.pc = address
      cpu.penaliseBranchCycles(address)
    } else {
      cpu.pc += 1
    }
  }

  /**
   * Branch if P.C is set.
   */
  def BCS(address: Int): Unit = {
    if (cpu.p.carry()) {
      cpu.pc = address
      cpu.penaliseBranchCycles(address)
    } else {
      cpu.pc += 1
    }
  }

  /**
   * Branch if P.Z is set
   *
   * @param address
   */
  def BEQ(address: Int): Unit = {
    if (cpu.p.zero()) {
      cpu.pc = address
      cpu.penaliseBranchCycles(address)
    } else {
      cpu.pc += 1
    }
  }

  /**
   * Bit test
   *
   * @param address
   */
  def BIT(address: Int): Unit = {
    val v = cpu.ram.read(address)
    val result = (cpu.acc & v).toByte
    cpu.p.testAndSetNegative(result)
    cpu.p.testAndSetOverflow(result)
    cpu.p.testAndSetZero(result)
  }

  /**
   * Branch if minus
   *
   * @param address
   */
  def BMI(address: Int): Unit = {
    if (cpu.p.sign()) {
      cpu.pc = address
      cpu.penaliseBranchCycles(address)
    } else {
      cpu.pc += 1
    }
  }

  /**
   * Branch if not equal
   *
   * @param address
   */
  def BNE(address: Int): Unit = {
    if (!cpu.p.zero()) {
      cpu.pc = address
      cpu.penaliseBranchCycles(address)
    } else {
      cpu.pc += 1
    }
  }

  /**
   * Branch if positive
   *
   * @param address
   */
  def BPL(address: Int): Unit = {
    if (!cpu.p.sign()) {
      cpu.pc = address
      cpu.penaliseBranchCycles(address)
    } else {
      cpu.pc += 1
    }
  }

  /**
   * The BRK instruction forces the generation of an interrupt request.
   * The program counter and processor status are pushed on the stack
   *  then the IRQ interrupt vector at $FFFE/F is loaded into the PC and the break flag
   *  in the status set to one.
   *
   * 1. Push address of BRK instruction + 2
   * 2. PHP
   * 3. SEI
   * 4. JMP ($FFFE)
   *
   */
  def BRK(): Unit = {
    cpu.pc += 1
    val (high, low) = BitUtils.unpackWord(cpu.pc)
    cpu.pushStack(high)
    cpu.pushStack(low)

    //PHP()
    //SEI()

    // set brk bit to 1
    cpu.p.setBrk(true)

    val h = cpu.ram.read(0xffff)
    val l = cpu.ram.read(0xfffe)

    cpu.pc = BitUtils.makeWord(l.toByte, h.toByte)
  }

  /**
   * Branch if overflow clear
   *
   * @param address
   */
  def BVC(address: Int): Unit = {
    if (!cpu.p.overflow()) {
      cpu.pc = address
      cpu.penaliseBranchCycles(address)
    } else {
      cpu.pc += 1
    }
  }

  /**
   * Branch if overflow set
   *
   * @param address
   */
  def BVS(address: Int): Unit = {
    if (cpu.p.overflow()) {
      cpu.pc = address
      cpu.penaliseBranchCycles(address)
    } else {
      cpu.pc += 1
    }
  }

  /**
   * Clear carry flag
   */
  def CLC(): Unit = {
    cpu.p.setCarry(false)
  }

  def CLD(): Unit = {
    cpu.p.setDecimal(false)
  }

  def CLI(): Unit = {
    cpu.p.setInterrupt(false)
  }

  def CLV(): Unit = {
    cpu.p.setOverflow(false)
  }

  /**
   * Compare two bytes
   *
   * @param a
   * @param b
   */
  def compare(a: Int, b: Int): Unit = {
    val c = a - b

    cpu.p.setZero(c == 0)
    cpu.p.setSign(c < 0)
    cpu.p.testAndSetCarryMinus(c)
  }

  /**
   * Compare with Accumulator
   *
   * @param address
   */
  def CMP(address: Int): Unit = {
    val v = cpu.ram.read(address)
    compare(cpu.acc, v)
  }

  /**
   * Compare with X register
   *
   * @param address
   */
  def CPX(address: Int): Unit = {
    val v = cpu.ram.read(address)
    compare(cpu.x, v)
  }

  /**
   * Compare with Y register
   *
   * @param address
   */
  def CPY(address: Int): Unit = {
    val v = cpu.ram.read(address)
    compare(cpu.y, v)
  }

  /**
   * Decrement memory
   *
   * @param address
   */
  def DEC(address: Int): Unit = {
    val v = cpu.ram.read(address)
    val result = v - 1

    cpu.ram.write(address, result.toByte)
    cpu.p.setSign(result < 0)
    cpu.p.setZero(result == 0)
  }

  /**
   * Decrement x register
   */
  def DEX(): Unit = {
    cpu.x = (cpu.x - 1).toByte
    cpu.p.setZero(cpu.x == 0)
    // XXX
    cpu.p.testAndSetNegative(cpu.x)
  }

  def DEY(): Unit = {
    cpu.y = (cpu.y - 1).toByte
    cpu.p.setZero(cpu.y == 0)
    cpu.p.testAndSetNegative(cpu.y)
  }

  /**
   * Exclusive OR
   *
   * @param address
   */
  def EOR(address: Int): Unit = {
    val v = cpu.ram.read(address)
    cpu.acc = (cpu.acc ^ v).toByte
    cpu.p.testAndSetZero(cpu.acc)
    cpu.p.testAndSetNegative(cpu.acc)
  }

  /**
   * Increment memory
   *
   * @param address
   */
  def INC(address: Int): Unit = {
    val v = cpu.ram.read(address)
    val result = (v + 1).toByte
    cpu.p.testAndSetZero(result)
    cpu.p.testAndSetNegative(result)

    cpu.ram.write(address, result)
  }

  def INX(): Unit = {
    cpu.x = (cpu.x - 1).toByte
    cpu.p.testAndSetZero(cpu.x)
    cpu.p.testAndSetNegative(cpu.x)
  }

  def INY(): Unit = {
    cpu.y = (cpu.y - 1).toByte
    cpu.p.testAndSetZero(cpu.y)
    cpu.p.testAndSetNegative(cpu.y)
  }

  /**
   * Jump to ~
   *
   * @param address
   */
  def JMP(address: Int): Unit = {
    cpu.pc = address
  }

  /**
   * Jump to subroutine
   *
   * @param address
   */
  def JSR(address: Int): Unit = {
    val (high, low) = BitUtils.unpackWord(cpu.pc - 1)
    cpu.pushStack(high)
    cpu.pushStack(low)
    cpu.pc = address
  }

  /**
   * Load accumulator
   *
   * @param address
   */
  def LDA(address: Int): Unit = {
    val v = cpu.ram.read(address)
    cpu.acc = v.toByte

    cpu.p.testAndSetZero(cpu.acc)
    cpu.p.testAndSetNegative(cpu.acc)
  }

  def LDX(address: Int): Unit = {
    val v = cpu.ram.read(address)
    cpu.x = v.toByte

    cpu.p.testAndSetZero(cpu.x)
    cpu.p.testAndSetNegative(cpu.x)
  }

  def LDY(address: Int): Unit = {
    val v = cpu.ram.read(address)
    cpu.y = v.toByte

    cpu.p.testAndSetZero(cpu.y)
    cpu.p.testAndSetNegative(cpu.y)
  }

  /**
   * Logic shift right
   *
   * @param address
   */
  def LSR(address: Int): Unit = {
    val v = cpu.ram.read(address)
    val result = (v >> 1).toByte
    cpu.ram.write(address, result)

    cpu.p.testAndSetZero(result)
    cpu.p.testAndSetNegative(result)
  }

  def LSR_acc(): Unit = {
    cpu.acc = (cpu.acc >> 1).toByte
    cpu.p.testAndSetZero(cpu.acc)
    cpu.p.testAndSetNegative(cpu.acc)
  }

  def NOP(): Unit = { }

  /**
   * Logic Inclusive OR
   *
   * @param address
   */
  def ORA(address: Int): Unit = {
    val v = cpu.ram.read(address)
    val result = (cpu.acc | v).toByte
    cpu.ram.write(address, result)

    cpu.p.testAndSetZero(result)
    cpu.p.testAndSetNegative(result)
  }

  /**
   * Push accumulator
   */
  def PHA(): Unit = {
    cpu.pushStack(cpu.acc)
  }

  /**
   * Push status register
   */
  def PHP(): Unit = {
    cpu.pushStack(cpu.p.v)
  }

  /**
   * Pull accumulator
   */
  def PLA(): Unit = {
    cpu.acc = cpu.pullStack().toByte
  }

  /**
   * Pull status register
   */
  def PLP(): Unit = {
    cpu.p.v = cpu.pullStack().toByte
  }

  /**
   * Rotate left
   *
   * @param address
   */
  def ROL(address: Int): Unit = {
    var v = cpu.ram.read(address)
    val tempCarry = v & 0x80
    v = v << 1
    if (cpu.p.carry()) v += 1
    cpu.p.setCarry(tempCarry > 0x0)

    val result = v.toByte
    cpu.ram.write(address, result)
    cpu.p.testAndSetZero(result)
    cpu.p.testAndSetNegative(result)
  }

  def ROL_acc(): Unit = {
    val tempCarry = cpu.acc & 0x80
    var v = cpu.acc << 1
    if (cpu.p.carry()) v += 1
    cpu.p.setCarry(tempCarry > 0x0)

    cpu.acc = v.toByte
    cpu.p.testAndSetZero(cpu.acc)
    cpu.p.testAndSetNegative(cpu.acc)
  }

  /**
   * Rotate right
   *
   * @param address
   */
  def ROR(address: Int): Unit = {
    var v = cpu.ram.read(address)
    val tempCarry = v & 0x1
    v = v >> 1
    if (cpu.p.carry()) v += 0x80
    cpu.p.setCarry(tempCarry > 0x0)

    val result = v.toByte
    cpu.ram.write(address, result)
    cpu.p.testAndSetZero(result)
    cpu.p.testAndSetNegative(result)
  }

  def ROR_acc(): Unit = {
    val tempCarry = cpu.acc & 0x80
    var v = cpu.acc >> 1
    if (cpu.p.carry()) v += 0x80
    cpu.p.setCarry(tempCarry > 0x0)

    cpu.acc = v.toByte
    cpu.p.testAndSetZero(cpu.acc)
    cpu.p.testAndSetNegative(cpu.acc)
  }

  /**
   * Return from interrupt
   */
  def RTI(): Unit = {
    PLP()

    val low = cpu.pullStack()
    val high = cpu.pullStack()

    cpu.pc = BitUtils.makeWord(low.toByte, high.toByte)
  }

  /**
   * Return from subroutine
   */
  def RTS(): Unit = {
    val low = cpu.pullStack()
    val high = cpu.pullStack()

    cpu.pc = BitUtils.makeWord(low.toByte, high.toByte)
  }

  /**
   * Substract with carry
   *
   * @param address
   */
  def SBC(address: Int): Unit = {
    val v = cpu.ram.read(address)

    val tmp = cpu.acc
    cpu.acc = (tmp - v).toByte

    val carryValue = if (cpu.p.carry()) 1 else 0
    cpu.acc = (cpu.acc - (1 - carryValue)).toByte
    cpu.p.testAndSetNegative(cpu.acc)
    cpu.p.testAndSetZero(cpu.acc)
    cpu.p.testAndSetOverflowMinus(tmp, cpu.acc)
    cpu.p.testAndSetCarryMinus(tmp - v - (1 - carryValue))
  }

  /**
   * Set carry
   */
  def SEC(): Unit = {
    cpu.p.setCarry(true)
  }

  /**
   * Set decimal
   */
  def SED(): Unit = {
    cpu.p.setDecimal(true)
  }

  /**
   * Set interrupt
   */
  def SEI(): Unit = {
    cpu.p.setInterrupt(true)
  }

  /**
   * Store accumulator
   *
   * @param address
   */
  def STA(address: Int): Unit = {
    cpu.ram.write(address, cpu.acc)
  }

  /**
   * Store x register
   *
   * @param address
   */
  def STX(address: Int): Unit = {
    cpu.ram.write(address, cpu.x)
  }

  /**
   * Store y register
   *
   * @param address
   */
  def STY(address: Int): Unit = {
    cpu.ram.write(address, cpu.y)
  }

  /**
   * Transfer accumulator to x
   */
  def TAX(): Unit = {
    cpu.x = cpu.acc
    cpu.p.testAndSetZero(cpu.x)
    cpu.p.testAndSetNegative(cpu.x)
  }

  def TAY(): Unit = {
    cpu.y = cpu.acc
    cpu.p.testAndSetZero(cpu.y)
    cpu.p.testAndSetNegative(cpu.y)
  }

  /**
   * Transfer stack pointer to x
   */
  def TSX(): Unit = {
    cpu.x = cpu.sp
    cpu.p.testAndSetZero(cpu.x)
    cpu.p.testAndSetNegative(cpu.x)
  }

  /**
   * Transfer x to accumulator
   */
  def TXA(): Unit = {
    cpu.acc = cpu.x
    cpu.p.testAndSetZero(cpu.acc)
    cpu.p.testAndSetNegative(cpu.acc)
  }

  /**
   * Transfer x to stack pointer
   */
  def TXS(): Unit = {
    cpu.sp = cpu.x
  }

  def TYA(): Unit = {
    cpu.acc = cpu.y
    cpu.p.testAndSetZero(cpu.acc)
    cpu.p.testAndSetNegative(cpu.acc)
  }
}

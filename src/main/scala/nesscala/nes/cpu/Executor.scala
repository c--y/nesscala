package nesscala.nes.cpu

import nesscala.nes.{M, Cpu}

import nesscala.util.{BitUtils, IntUtils}

/**
 * Opcode executor
 *
 * Created by chenyan on 15-6-1.
 */
class Executor(cpu: Cpu) {

  val NoneAddressParam: Int = -1

  val fnTable: Array[Function0[Unit]] = new Array(0xff)

  // Initialize the function table
  // Implied
  //fnTable(0x00) = new Opcode(BRK, 7, null, -1)
  fnTable(0x00) = new Opcode(cpu, BRK, null, 7, null, null, null)
  fnTable(0x01) = new Opcode(cpu, null, ORA _, 6, cpu.indirectIndexedAddress, null, null)
  fnTable(0x05) = new Opcode(cpu, null, ORA _, 2, cpu.zeroPageAddress, null, null)
  fnTable(0x06) = new Opcode(cpu, null, ASL _, 5, cpu.zeroPageAddress, null, null)
  // Implied
  fnTable(0x08) = new Opcode(cpu, PHP, null, 3, null, null, null)
  fnTable(0x09) = new Opcode(cpu, null, ORA _, 2, cpu.immediateAddress, null, null)
  // Accumulator
  fnTable(0x0a) = new Opcode(cpu, ASL_acc, null, 2, null, null, null)
  fnTable(0x0d) = new Opcode(cpu, null, ORA _, 4, cpu.absoluteAddress, null, null)
  fnTable(0x0e) = new Opcode(cpu, null, ASL _, 6, cpu.absoluteAddress, null, null)
  // Implied
  fnTable(0x10) = new Opcode(cpu, null, BPL _, 2, cpu.relativeAddress, null, null)
  fnTable(0x11) = new Opcode(cpu, null, ORA _, 5, cpu.indirectIndexedAddress, null, null)
  fnTable(0x15) = new Opcode(cpu, null, ORA _, 3, null, cpu.zeroPageIndexedAddress, 'X)
  fnTable(0x16) = new Opcode(cpu, null, ASL _, 6, null, cpu.zeroPageIndexedAddress, 'X)
  fnTable(0x18) = new Opcode(cpu, CLC, null, 2, null, null, null)
  fnTable(0x19) = new Opcode(cpu, null, ORA _, 4, null, cpu.absoluteIndexedAddress, 'Y)
  fnTable(0x1d) = new Opcode(cpu, null, ORA _, 4, null, cpu.absoluteIndexedAddress, 'X)
  fnTable(0x1e) = new Opcode(cpu, null, ASL _, 7, null, cpu.absoluteIndexedAddress, 'X)
  fnTable(0x20) = new Opcode(cpu, null, JSR _, 6, cpu.absoluteAddress, null, null)
  fnTable(0x21) = new Opcode(cpu, null, AND _, 6, cpu.indirectIndexedAddress, null, null)
  fnTable(0x24) = new Opcode(cpu, null, BIT _, 3, cpu.zeroPageAddress, null, null)
  fnTable(0x25) = new Opcode(cpu, null, AND _, 2, cpu.zeroPageAddress, null, null)
  fnTable(0x26) = new Opcode(cpu, null, ROL _, 5, cpu.zeroPageAddress, null, null)
  // Implied
  fnTable(0x28) = new Opcode(cpu, PLP, null, 4, null, null, null)
  fnTable(0x29) = new Opcode(cpu, null, AND _, 2, cpu.immediateAddress, null, null)
  // Accumulator
  fnTable(0x2a) = new Opcode(cpu, ROL_acc, null, 2, null, null, null)
  fnTable(0x2c) = new Opcode(cpu, null, BIT _, 4, cpu.absoluteAddress, null, null)
  fnTable(0x2d) = new Opcode(cpu, null, AND _, 4, cpu.absoluteAddress, null, null)
  fnTable(0x2e) = new Opcode(cpu, null, ROL _, 6, cpu.absoluteAddress, null, null)
  fnTable(0x30) = new Opcode(cpu, null, BMI _, 2, cpu.relativeAddress, null, null)
  fnTable(0x31) = new Opcode(cpu, null, AND _, 5, cpu.indirectIndexedAddress, null, null)
  fnTable(0x35) = new Opcode(cpu, null, AND _, 3, null, cpu.zeroPageIndexedAddress, 'X)
  fnTable(0x36) = new Opcode(cpu, null, ROL _, 6, null, cpu.zeroPageIndexedAddress, 'X)
  // Implied
  fnTable(0x38) = new Opcode(cpu, SEC, null, 2, null, null, null)
  fnTable(0x39) = new Opcode(cpu, null, AND _, 4, null, cpu.absoluteIndexedAddress, 'Y)
  fnTable(0x3d) = new Opcode(cpu, null, AND _, 4, null, cpu.absoluteIndexedAddress, 'X)
  fnTable(0x3e) = new Opcode(cpu, null, ROL _, 7, null, cpu.absoluteIndexedAddress, 'X)
  // Implied
  fnTable(0x40) = new Opcode(cpu, RTI, null, 4, null, null, null)
  fnTable(0x41) = new Opcode(cpu, null, EOR _, 6, cpu.indirectIndexedAddress, null, null)
  fnTable(0x45) = new Opcode(cpu, null, EOR _, 3, cpu.zeroPageAddress, null, null)
  fnTable(0x46) = new Opcode(cpu, null, LSR _, 5, cpu.zeroPageAddress, null, null)
  // Implied
  fnTable(0x48) = new Opcode(cpu, PHA, null, 3, null, null, null)
  fnTable(0x49) = new Opcode(cpu, null, EOR _, 2, cpu.immediateAddress, null, null)
  // Accumulator
  fnTable(0x4a) = new Opcode(cpu, LSR_acc, null, 2, null, null, null)
  fnTable(0x4c) = new Opcode(cpu, null, JMP _, 3, cpu.absoluteAddress, null, null)
  fnTable(0x4d) = new Opcode(cpu, null, EOR _, 4, cpu.absoluteAddress, null, null)
  fnTable(0x4e) = new Opcode(cpu, null, LSR _, 6, cpu.absoluteAddress, null, null)
  fnTable(0x50) = new Opcode(cpu, null, BVC _, 2, cpu.relativeAddress, null, null)
  fnTable(0x51) = new Opcode(cpu, null, EOR _, 5, cpu.indirectIndexedAddress, null, null)
  fnTable(0x55) = new Opcode(cpu, null, EOR _, 4, null, cpu.zeroPageIndexedAddress, 'X)

  fnTable(0x56) = new Opcode(cpu, null, LSR _, 6, null, cpu.zeroPageIndexedAddress _, 'X)
  // Implied
  fnTable(0x58) = new Opcode(cpu, CLI, null, 2, null, null, null)
  fnTable(0x59) = new Opcode(cpu, null, EOR _, 4, null, cpu.absoluteIndexedAddress, 'Y)
  fnTable(0x5d) = new Opcode(cpu, null, EOR _, 6, null, cpu.absoluteIndexedAddress, 'X)
  fnTable(0x5e) = new Opcode(cpu, null, LSR _, 7, null, cpu.absoluteIndexedAddress, 'X)

  fnTable(0x60) = new Opcode(cpu, RTS, null, 6, null, null, null)
  fnTable(0x61) = new Opcode(cpu, null, LSR _, 6, cpu.absoluteAddress, null, null)
  fnTable(0x65) = new Opcode(cpu, null, ADC _, 3, cpu.zeroPageAddress, null, null)
  fnTable(0x66) = new Opcode(cpu, null, ROR _, 5, cpu.zeroPageAddress, null, null)
  fnTable(0x68) = new Opcode(cpu, PLA, null, 6, null, null, null)
  fnTable(0x69) = new Opcode(cpu, null, ADC _, 2, cpu.immediateAddress, null, null)
  // Accumulator
  fnTable(0x6a) = new Opcode(cpu, ROR_acc, null, 2, null, null, null)
  fnTable(0x6c) = new Opcode(cpu, null, JMP _, 5, cpu.indirectAddress, null, null)
  fnTable(0x6d) = new Opcode(cpu, null, ADC _, 4, cpu.absoluteAddress, null, null)
  fnTable(0x6e) = new Opcode(cpu, null, ROR _, 6, cpu.absoluteAddress, null, null)
  fnTable(0x70) = new Opcode(cpu, null, BVS _, 2, cpu.relativeAddress, null, null)
  fnTable(0x71) = new Opcode(cpu, null, ADC _, 5, cpu.absoluteAddress, null, null)
  fnTable(0x75) = new Opcode(cpu, null, ADC _, 6, null, cpu.zeroPageIndexedAddress, 'X)
  fnTable(0x76) = new Opcode(cpu, null, ROR _, 6, null, cpu.zeroPageIndexedAddress, 'X)
  // Implied
  fnTable(0x78) = new Opcode(cpu, SEI, null, 2, null, null, null)
  fnTable(0x79) = new Opcode(cpu, null, ADC _, 4, null, cpu.absoluteIndexedAddress, 'Y)
  fnTable(0x7d) = new Opcode(cpu, null, ADC _, 4, null, cpu.absoluteIndexedAddress, 'X)
  fnTable(0x7e) = new Opcode(cpu, null, ROR _, 7, null, cpu.absoluteIndexedAddress, 'X)
  fnTable(0x81) = new Opcode(cpu, null, STA _, 6, cpu.indirectIndexedAddress, null, null)
  fnTable(0x84) = new Opcode(cpu, null, STY _, 3, cpu.zeroPageAddress, null, null)
  fnTable(0x85) = new Opcode(cpu, null, STA _, 3, cpu.zeroPageAddress, null, null)
  fnTable(0x86) = new Opcode(cpu, null, STX _, 6, cpu.zeroPageAddress, null, null)
  // Implied
  fnTable(0x88) = new Opcode(cpu, DEY, null, 2, null, null, null)
  fnTable(0x8a) = new Opcode(cpu, TXA, null, 2, null, null, null)
  fnTable(0x8c) = new Opcode(cpu, null, STY _, 4, cpu.absoluteAddress, null, null)
  fnTable(0x8d) = new Opcode(cpu, null, STA _, 4, cpu.absoluteAddress, null, null)
  fnTable(0x8e) = new Opcode(cpu, null, STX _, 4, cpu.absoluteAddress, null, null)
  fnTable(0x90) = new Opcode(cpu, null, BCC _, 2, cpu.relativeAddress, null, null)
  fnTable(0x91) = new Opcode(cpu, null, STA _, 6, cpu.indirectIndexedAddress, null, null)
  fnTable(0x94) = new Opcode(cpu, null, STY _, 4, null, cpu.zeroPageIndexedAddress, 'X)
  fnTable(0x95) = new Opcode(cpu, null, STA _, 4, null, cpu.zeroPageIndexedAddress, 'X)
  fnTable(0x96) = new Opcode(cpu, null, STX _, 4, null, cpu.zeroPageIndexedAddress, 'Y)
  // Implied
  fnTable(0x98) = new Opcode(cpu, TYA, null, 2, null, null, null)
  fnTable(0x99) = new Opcode(cpu, null, STA _, 5, null, cpu.absoluteIndexedAddress, 'Y)
  fnTable(0x9a) = new Opcode(cpu, TXS, null, 2, null, null, null)
  fnTable(0x9d) = new Opcode(cpu, null, STA _, 6, null, cpu.absoluteIndexedAddress, 'X)
  fnTable(0xa0) = new Opcode(cpu, null, LDY _, 2, cpu.immediateAddress, null, null)
  fnTable(0xa1) = new Opcode(cpu, null, LDA _, 6, cpu.indirectIndexedAddress, null, null)
  fnTable(0xa2) = new Opcode(cpu, null, LDX _, 2, cpu.immediateAddress, null, null)
  fnTable(0xa4) = new Opcode(cpu, null, LDY _, 3, cpu.zeroPageAddress, null, null)
  fnTable(0xa5) = new Opcode(cpu, null, LDA _, 3, cpu.zeroPageAddress, null, null)
  fnTable(0xa6) = new Opcode(cpu, null, LDX _, 3, cpu.zeroPageAddress, null, null)
  // Implied
  fnTable(0xa8) = new Opcode(cpu, TAY, null, 2, null, null, null)
  fnTable(0xa9) = new Opcode(cpu, null, LDA _, 2, cpu.immediateAddress, null, null)
  fnTable(0xaa) = new Opcode(cpu, TAX, null, 2, null, null, null)
  fnTable(0xac) = new Opcode(cpu, null, LDY _, 4, cpu.absoluteAddress, null, null)
  fnTable(0xad) = new Opcode(cpu, null, LDA _, 4, cpu.absoluteAddress, null, null)
  fnTable(0xae) = new Opcode(cpu, null, LDX _, 4, cpu.absoluteAddress, null, null)
  fnTable(0xb0) = new Opcode(cpu, null, BCS _, 2, cpu.relativeAddress, null, null)
  fnTable(0xb1) = new Opcode(cpu, null, LDA _, 5, cpu.indirectIndexedAddress, null, null)
  fnTable(0xb4) = new Opcode(cpu, null, LDY _, 4, null, cpu.zeroPageIndexedAddress, 'X)
  fnTable(0xb5) = new Opcode(cpu, null, LDA _, 4, null, cpu.zeroPageIndexedAddress, 'X)
  fnTable(0xb6) = new Opcode(cpu, null, LDX _, 4, null, cpu.zeroPageIndexedAddress, 'Y)
  // Implied
  fnTable(0xb8) = new Opcode(cpu, CLV, null, 2, null, null, null)
  fnTable(0xb9) = new Opcode(cpu, null, LDA _, 6, null, cpu.absoluteIndexedAddress, 'Y)
  // Implied
  fnTable(0xba) = new Opcode(cpu, TSX, null, 2, null, null, null)
  fnTable(0xbc) = new Opcode(cpu, null, LDY _, 4, null, cpu.absoluteIndexedAddress, 'X)
  fnTable(0xbd) = new Opcode(cpu, null, LDA _, 6, null, cpu.absoluteIndexedAddress, 'X)
  fnTable(0xbe) = new Opcode(cpu, null, LDX _, 4, null, cpu.absoluteIndexedAddress, 'Y)
  fnTable(0xc0) = new Opcode(cpu, null, CPY _, 2, cpu.immediateAddress, null, null)
  fnTable(0xc1) = new Opcode(cpu, null, CMP _, 2, cpu.indirectIndexedAddress, null, null)
  fnTable(0xc4) = new Opcode(cpu, null, CPY _, 3, cpu.zeroPageAddress, null, null)
  fnTable(0xc5) = new Opcode(cpu, null, CMP _, 3, cpu.zeroPageAddress, null, null)
  fnTable(0xc6) = new Opcode(cpu, null, DEC _, 5, cpu.zeroPageAddress, null, null)
  // Implied
  fnTable(0xc8) = new Opcode(cpu, INY, null, 2, null, null, null)
  fnTable(0xc9) = new Opcode(cpu, null, CMP _, 2, cpu.immediateAddress, null, null)
  fnTable(0xca) = new Opcode(cpu, DEX, null, 2, null, null, null)
  fnTable(0xcc) = new Opcode(cpu, null, CPY _, 4, cpu.absoluteAddress, null, null)
  fnTable(0xcd) = new Opcode(cpu, null, CMP _, 4, cpu.absoluteAddress, null, null)
  fnTable(0xce) = new Opcode(cpu, null, DEC _, 6, cpu.absoluteAddress, null, null)
  fnTable(0xd0) = new Opcode(cpu, null, BNE _, 2, cpu.relativeAddress, null, null)
  fnTable(0xd1) = new Opcode(cpu, null, BNE _, 5, cpu.relativeAddress, null, null)
  fnTable(0xd5) = new Opcode(cpu, null, CMP _, 6, null, cpu.zeroPageIndexedAddress, 'X)
  fnTable(0xd6) = new Opcode(cpu, null, DEC _, 6, null, cpu.zeroPageIndexedAddress, 'X)
  // Implied
  fnTable(0xd8) = new Opcode(cpu, CLD, null, 2, null, null, null)
  fnTable(0xd9) = new Opcode(cpu, null, CMP _, 4, null, cpu.absoluteIndexedAddress, 'Y)
  fnTable(0xdd) = new Opcode(cpu, null, CMP _, 4, null, cpu.absoluteIndexedAddress, 'X)
  fnTable(0xde) = new Opcode(cpu, null, DEC _, 7, null, cpu.absoluteIndexedAddress, 'X)
  fnTable(0xe0) = new Opcode(cpu, null, CPX _, 2, cpu.immediateAddress, null, null)
  fnTable(0xe1) = new Opcode(cpu, null, SBC _, 6, cpu.indirectIndexedAddress, null, null)
  fnTable(0xe4) = new Opcode(cpu, null, CPX _, 3, cpu.zeroPageAddress, null, null)
  fnTable(0xe5) = new Opcode(cpu, null, SBC _, 3, cpu.zeroPageAddress, null, null)
  fnTable(0xe6) = new Opcode(cpu, null, INC _, 5, cpu.zeroPageAddress, null, null)
  // Implied
  fnTable(0xe8) = new Opcode(cpu, INX, null, 2, null, null, null)
  fnTable(0xe9) = new Opcode(cpu, null, SBC _, 2, cpu.immediateAddress, null, null)
  // Implied
  fnTable(0xea) = new Opcode(cpu, NOP, null, 2, null, null, null)
  fnTable(0xec) = new Opcode(cpu, null, CPX _, 4, cpu.absoluteAddress, null, null)
  fnTable(0xed) = new Opcode(cpu, null, SBC _, 4, cpu.absoluteAddress, null, null)
  fnTable(0xee) = new Opcode(cpu, null, INC _, 6, cpu.absoluteAddress, null, null)
  fnTable(0xf0) = new Opcode(cpu, null, BEQ _, 2, cpu.relativeAddress, null, null)
  fnTable(0xf1) = new Opcode(cpu, null, SBC _, 5, cpu.indirectIndexedAddress, null, null)
  fnTable(0xf5) = new Opcode(cpu, null, SBC _, 4, null, cpu.zeroPageIndexedAddress, 'X)
  fnTable(0xf6) = new Opcode(cpu, null, INC _, 6, null, cpu.zeroPageIndexedAddress, 'X)
  // Implied
  fnTable(0xf8) = new Opcode(cpu, SED, null, 2, null, null, null)
  fnTable(0xf9) = new Opcode(cpu, null, SBC _, 4, null, cpu.absoluteIndexedAddress, 'Y)
  fnTable(0xfd) = new Opcode(cpu, null, SBC _, 4, null, cpu.absoluteIndexedAddress, 'X)
  fnTable(0xfe) = new Opcode(cpu, null, INC _, 7, null, cpu.absoluteIndexedAddress, 'X)

  // Basic operations
  def ADC(address: Int): Unit = {
    val v = M.ram.read(address)
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
    val v = M.ram.read(address)
    cpu.acc = (cpu.acc & v).toByte

    cpu.p.testAndSetNegative(cpu.acc)
    cpu.p.testAndSetZero(cpu.acc)
  }

  def ASL(address: Int): Unit = {
    val v = M.ram.read(address)
    cpu.p.setCarry((v & 0x80) > 0)

    val shiftV = (v << 1).toByte
    M.ram.write(address, shiftV)

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
    if (!cpu.p.carry()) {
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
    val v = M.ram.read(address)
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

    val h = M.ram.read(0xffff)
    val l = M.ram.read(0xfffe)

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
    val v = M.ram.read(address)
    compare(cpu.acc, v)
  }

  /**
   * Compare with X register
   *
   * @param address
   */
  def CPX(address: Int): Unit = {
    val v = M.ram.read(address)
    compare(cpu.x, v)
  }

  /**
   * Compare with Y register
   *
   * @param address
   */
  def CPY(address: Int): Unit = {
    val v = M.ram.read(address)
    compare(cpu.y, v)
  }

  /**
   * Decrement memory
   *
   * @param address
   */
  def DEC(address: Int): Unit = {
    val v = M.ram.read(address)
    val result = v - 1

    M.ram.write(address, result.toByte)
    cpu.p.setSign(result < 0)
    cpu.p.setZero(result == 0)
  }

  /**
   * Decrement x register
   */
  def DEX(): Unit = {
    cpu.x = (IntUtils.toUnsigned(cpu.x) - 1).toByte
    cpu.p.setZero(cpu.x == 0)
    // XXX
    cpu.p.testAndSetNegative(cpu.x)
  }

  def DEY(): Unit = {
    cpu.y = (IntUtils.toUnsigned(cpu.y) - 1).toByte
    cpu.p.setZero(cpu.y == 0)
    cpu.p.testAndSetNegative(cpu.y)
  }

  /**
   * Exclusive OR
   *
   * @param address
   */
  def EOR(address: Int): Unit = {
    val v = M.ram.read(address)
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
    val v = M.ram.read(address)
    val result = (v + 1).toByte
    cpu.p.testAndSetZero(result)
    cpu.p.testAndSetNegative(result)

    M.ram.write(address, result)
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
    // TODO need -1 ? cpu.pc - 1?
    val (high, low) = BitUtils.unpackWord(cpu.pc)
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
    val v = M.ram.read(address)
    cpu.acc = v.toByte

    cpu.p.testAndSetZero(cpu.acc)
    cpu.p.testAndSetNegative(cpu.acc)
  }

  def LDX(address: Int): Unit = {
    val v = M.ram.read(address)
    cpu.x = v.toByte

    cpu.p.testAndSetZero(cpu.x)
    cpu.p.testAndSetNegative(cpu.x)
  }

  def LDY(address: Int): Unit = {
    val v = M.ram.read(address)
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
    val v = M.ram.read(address)
    val result = (v >> 1).toByte
    M.ram.write(address, result)

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
    val v = M.ram.read(address)
    val result = (cpu.acc | v).toByte
    M.ram.write(address, result)

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
    var v = M.ram.read(address)
    val tempCarry = v & 0x80
    v = v << 1
    if (cpu.p.carry()) v += 1
    cpu.p.setCarry(tempCarry > 0x0)

    val result = v.toByte
    M.ram.write(address, result)
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
    var v = M.ram.read(address)
    val tempCarry = v & 0x1
    v = v >> 1
    if (cpu.p.carry()) v += 0x80
    cpu.p.setCarry(tempCarry > 0x0)

    val result = v.toByte
    M.ram.write(address, result)
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
    val v = M.ram.read(address)

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
    M.ram.write(address, cpu.acc)
  }

  /**
   * Store x register
   *
   * @param address
   */
  def STX(address: Int): Unit = {
    M.ram.write(address, cpu.x)
  }

  /**
   * Store y register
   *
   * @param address
   */
  def STY(address: Int): Unit = {
    M.ram.write(address, cpu.y)
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

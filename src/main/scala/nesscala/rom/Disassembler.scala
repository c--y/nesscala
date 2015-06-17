package nesscala.rom

import java.io.{File, FileOutputStream}

import nesscala.nes.{M, Cpu}
import nesscala.util.{IntUtils, BitUtils}

/**
 * Created by chenyan on 15-5-31.
 */
object Disassembler {

  var cpu: Cpu = null

  var pc = 0

  var readBytesLatch = false;

  var readBytes = ""

  def immediateMode(): Int = {
    readBytesLatch = true;
    val operand = M.ram.read(pc)
    readBytes = "%02X      ".format(M.ram.read(pc))
    //pc
    operand
  }

  def absoluteMode(): Int = {
    val low = M.ram.read(pc).toByte
    val high = M.ram.read(pc + 1).toByte
    readBytesLatch = true;
    readBytes = "%02X %02X   ".format(low, high)
    BitUtils.makeWord(low, high)
  }

  def zeroPageMode(): Int = {
    val address = M.ram.read(pc)
    readBytesLatch = true;
    readBytes = "%02X      ".format(address)
    address
  }

  def indirectAbsoluteMode(): Int = {
    val low = M.ram.read(pc).toByte
    val high = M.ram.read(pc + 1).toByte
    val ret = BitUtils.makeWord(low, high)
    pc += 1
    readBytesLatch = true;
    readBytes = "%02X %02X  ".format(low, high)
    ret
  }

  def absoluteIndexedMode(index: Int): Int = {
    val low = M.ram.read(pc).toByte
    val high = M.ram.read(pc + 1).toByte
    val ret = BitUtils.makeWord(low, high) + index
    readBytesLatch = true;
    readBytes = "%02X %02X  ".format(low, high)
    ret
  }

  def zeroPageIndexedMode(index: Int): Int = {
    val addr = M.ram.read(pc).toByte
    readBytesLatch = true;
    readBytes = "%02X      ".format(addr)
    addr + index
  }

  def indexedIndirectMode(): Int = {
    val byte = M.ram.read(pc)
    val addr = byte + IntUtils.toUnsigned(cpu.x)
    readBytesLatch = true;
    readBytes = "%02X      ".format(byte)
    BitUtils.makeWord(M.ram.read(addr).toByte, M.ram.read(addr + 1).toByte)
  }

  def indirectIndexedMode(): Int = {
    val addr = M.ram.read(pc)
    readBytesLatch = true;
    readBytes = "%02X      ".format(addr)
    BitUtils.makeWord(M.ram.read(addr).toByte, M.ram.read(addr + 1).toByte) + IntUtils.toUnsigned(cpu.y)
  }

  def relativeMode(): Int = {
    readBytesLatch = true;
    readBytes = "        "
    0
  }

  def accumulatorMode(): Int = {
    readBytesLatch = true;
    readBytes = "        "
    0
  }

  def generateDis(opcode: String, addressTuple: (Int, String)) =
    "%4X  %2X  %s  %s $%X"

  def dis(opcode: Int, p: Int, c: Cpu): String = {
    readBytesLatch = false
    this.cpu = c
    this.pc = p + 1
    print(f"$p%4X  $opcode%02X  ")

    var opcodeString = opcode match {
      /*
        MODE           SYNTAX       HEX LEN TIM
        Immediate     ADC #$44      $69  2   2
        Zero Page     ADC $44       $65  2   3
        Zero Page,X   ADC $44,X     $75  2   4
        Absolute      ADC $4400     $6D  3   4
        Absolute,X    ADC $4400,X   $7D  3   4+
        Absolute,Y    ADC $4400,Y   $79  3   4+
        Indirect,X    ADC ($44,X)   $61  2   6
        Indirect,Y    ADC ($44),Y   $71  2   5+
      */
      case 0x69 => f"ADC #$$%%02X".format(immediateMode()) // immediate
      case 0x65 => f"ADC $$%%X".format(zeroPageMode()) // zeroPage
      case 0x75 => f"ADC $$%%X".format(zeroPageIndexedMode(cpu.x))
      case 0x6d => f"ADC $$%%X".format(absoluteMode())
      case 0x7d => f"ADC $$%%X, X".format(absoluteIndexedMode(cpu.x))
      case 0x79 => f"ADC $$%%X, Y".format(absoluteIndexedMode(cpu.y))
      case 0x61 => f"ADC ($$%%X, X)".format(indexedIndirectMode())
      case 0x71 => f"ADC ($$%%X), Y".format(indirectIndexedMode())
      /*
        MODE           SYNTAX       HEX LEN TIM
        Immediate     AND #$44      $29  2   2
        Zero Page     AND $44       $25  2   2
        Zero Page,X   AND $44,X     $35  2   3
        Absolute      AND $4400     $2D  3   4
        Absolute,X    AND $4400,X   $3D  3   4+
        Absolute,Y    AND $4400,Y   $39  3   4+
        Indirect,X    AND ($44,X)   $21  2   6
        Indirect,Y    AND ($44),Y   $31  2   5+
      */
      case 0x29 => f"AND #$$%%02X".format(immediateMode())
      case 0x25 => f"AND $$%%X".format(zeroPageMode())
      case 0x35 => f"AND $$%%X, X".format(zeroPageIndexedMode(cpu.x))
      case 0x2d => f"AND $$%%X".format(absoluteMode())
      case 0x3d => f"AND $$%%X, X".format(absoluteIndexedMode(cpu.x))
      case 0x39 => f"AND $$%%X, X".format(absoluteIndexedMode(cpu.y))
      case 0x21 => f"AND ($$%%X, X)".format(indexedIndirectMode())
      case 0x31 => f"AND ($$%%X), Y".format(indirectIndexedMode())
      /*
        MODE           SYNTAX       HEX LEN TIM
        Accumulator   ASL A         $0A  1   2
        Zero Page     ASL $44       $06  2   5
        Zero Page,X   ASL $44,X     $16  2   6
        Absolute      ASL $4400     $0E  3   6
        Absolute,X    ASL $4400,X   $1E  3   7
      */
      case 0x0a => f"          ASL A"
      case 0x06 => f"ASL $$%%X".format(zeroPageMode())
      case 0x16 => f"ASL $$%%X, X".format(zeroPageIndexedMode(cpu.x))
      case 0x0e => f"ASL $$%%X".format(absoluteMode())
      case 0x1e => f"ASL $$%%X, X".format(absoluteIndexedMode(cpu.x))
      /*
        MNEMONIC                       HEX
        BPL (Branch on PLus)           $10
        BMI (Branch on MInus)          $30
        BVC (Branch on oVerflow Clear) $50
        BVS (Branch on oVerflow Set)   $70
        BCC (Branch on Carry Clear)    $90
        BCS (Branch on Carry Set)      $B0
        BNE (Branch on Not Equal)      $D0
        BEQ (Branch on EQual)          $F0
      */
      case 0x10 => f"BPL #$$%%02X".format(immediateMode())
      case 0x30 => f"BMI #$$%%02X".format(immediateMode())
      case 0x50 => f"BVC #$$%%02X".format(immediateMode())
      case 0x70 => f"BVS #$$%%02X".format(immediateMode())
      case 0x90 => f"BCC #$$%%02X".format(immediateMode())
      case 0xb0 => f"BCS #$$%%02X".format(immediateMode())
      case 0xd0 => f"BNE #$$%%02X".format(immediateMode())
      case 0xf0 => f"BEQ #$$%%02X".format(immediateMode())
      /*
        MODE           SYNTAX       HEX LEN TIM
        Zero Page     BIT $44       $24  2   3
        Absolute      BIT $4400     $2C  3   4
      */
      case 0x24 => f"BIT $$%%X".format(zeroPageMode())
      case 0x2c => f"BIT $$%%X".format(absoluteMode())
      /*
        MODE           SYNTAX       HEX LEN TIM
        Implied       BRK           $00  1   7
      */
      case 0x00 => "BRK"
      /*
        FLAG
        MNEMONIC                       HEX
        CLC (CLear Carry)              $18
        SEC (SEt Carry)                $38
        CLI (CLear Interrupt)          $58
        SEI (SEt Interrupt)            $78
        CLV (CLear oVerflow)           $B8
        CLD (CLear Decimal)            $D8
        SED (SEt Decimal)              $F8
       */
      case 0x18 => "CLC"
      case 0x38 => "SEC"
      case 0x58 => "CLI"
      case 0x78 => "SEI"
      case 0xb8 => "CLV"
      case 0xd8 => "CLD"
      case 0xf8 => "SED"
      /*
        MODE           SYNTAX       HEX LEN TIM
        Immediate     CMP #$44      $C9  2   2
        Zero Page     CMP $44       $C5  2   3
        Zero Page,X   CMP $44,X     $D5  2   4
        Absolute      CMP $4400     $CD  3   4
        Absolute,X    CMP $4400,X   $DD  3   4+
        Absolute,Y    CMP $4400,Y   $D9  3   4+
        Indirect,X    CMP ($44,X)   $C1  2   6
        Indirect,Y    CMP ($44),Y   $D1  2   5+
       */
      case 0xc9 => f"CMP #$$%%02X".format(immediateMode())
      case 0xc5 => f"CMP $$%%X".format(zeroPageMode())
      case 0xd5 => f"CMP $$%%X, X".format(zeroPageIndexedMode(cpu.x))
      case 0xcd => f"CMP $$%%X".format(absoluteMode())
      case 0xdd => f"CMP $$%%X, X".format(absoluteIndexedMode(cpu.x))
      case 0xd9 => f"CMP $$%%X, Y".format(absoluteIndexedMode(cpu.y))
      case 0xc1 => f"CMP ($$%%X, X)".format(indexedIndirectMode())
      case 0xd1 => f"CMP ($$%%X), Y".format(indirectIndexedMode())
      /*
        MODE           SYNTAX       HEX LEN TIM
        Immediate     CPX #$44      $E0  2   2
        Zero Page     CPX $44       $E4  2   3
        Absolute      CPX $4400     $EC  3   4
      */
      case 0xe0 => f"CPX #$$%%02X".format(immediateMode())
      case 0xe4 => f"CPX $$%%X".format(zeroPageMode())
      case 0xec => f"CPX $$%%X".format(absoluteMode())
      /*
        MODE           SYNTAX       HEX LEN TIM
        Immediate     CPY #$44      $C0  2   2
        Zero Page     CPY $44       $C4  2   3
        Absolute      CPY $4400     $CC  3   4
      */
      case 0xc0 => f"CPY #$$%%02X".format(immediateMode())
      case 0xc4 => f"CPY $$%%X".format(zeroPageMode())
      case 0xcc => f"CPY $$%%X".format(absoluteMode())
      /*
        MODE           SYNTAX       HEX LEN TIM
        Zero Page     DEC $44       $C6  2   5
        Zero Page,X   DEC $44,X     $D6  2   6
        Absolute      DEC $4400     $CE  3   6
        Absolute,X    DEC $4400,X   $DE  3   7
      */
      case 0xc6 => f"DEC $$%%X".format(zeroPageMode())
      case 0xd6 => f"DEC $$%%X, X".format(zeroPageIndexedMode(cpu.x))
      case 0xce => f"DEC $$%%X".format(absoluteMode())
      case 0xde => f"DEC $$%%X, X".format(absoluteIndexedMode(cpu.x))
      /*
        MNEMONIC                 HEX
        TAX (Transfer A to X)    $AA
        TXA (Transfer X to A)    $8A
        DEX (DEcrement X)        $CA
        INX (INcrement X)        $E8
        TAY (Transfer A to Y)    $A8
        TYA (Transfer Y to A)    $98
        DEY (DEcrement Y)        $88
        INY (INcrement Y)        $C8
      */
      case 0xaa => "TAX"
      case 0x8a => "TXA"
      case 0xca => "DEX"
      case 0xe8 => "INX"
      case 0xa8 => "TAY"
      case 0x98 => "TYA"
      case 0x88 => "DEY"
      case 0xc8 => "INY"
      /*
        MODE           SYNTAX       HEX LEN TIM
        Immediate     EOR #$44      $49  2   2
        Zero Page     EOR $44       $45  2   3
        Zero Page,X   EOR $44,X     $55  2   4
        Absolute      EOR $4400     $4D  3   4
        Absolute,X    EOR $4400,X   $5D  3   4+
        Absolute,Y    EOR $4400,Y   $59  3   4+
        Indirect,X    EOR ($44,X)   $41  2   6
        Indirect,Y    EOR ($44),Y   $51  2   5+
      */
      case 0x49 => f"EOR #$$%%02X".format(immediateMode())
      case 0x45 => f"EOR $$%%X".format(zeroPageMode())
      case 0x55 => f"EOR $$%%X, X".format(zeroPageIndexedMode(cpu.x))
      case 0x4d => f"EOR $$%%X".format(absoluteMode())
      case 0x5d => f"EOR $$%%X, X".format(absoluteIndexedMode(cpu.x))
      case 0x59 => f"EOR $$%%X, Y".format(absoluteIndexedMode(cpu.y))
      case 0x41 => f"EOR ($$%%X, X)".format(indexedIndirectMode())
      case 0x51 => f"EOR ($$%%X), Y".format(indirectIndexedMode())
      /*
        MODE           SYNTAX       HEX LEN TIM
        Zero Page     INC $44       $E6  2   5
        Zero Page,X   INC $44,X     $F6  2   6
        Absolute      INC $4400     $EE  3   6
        Absolute,X    INC $4400,X   $FE  3   7
      */
      case 0xe6 => f"INC $$%%X".format(zeroPageMode())
      case 0xf6 => f"INC $$%%X, X".format(zeroPageIndexedMode(cpu.x))
      case 0xee => f"INC $$%%X".format(absoluteMode())
      case 0xfe => f"INC $$%%X, X".format(absoluteIndexedMode(cpu.x))
      /*
        MODE           SYNTAX       HEX LEN TIM
        Absolute      JMP $5597     $4C  3   3
        Indirect      JMP ($5597)   $6C  3   5
      */
      case 0x4c => f"JMP $$%%X".format(absoluteMode())
      case 0x6c => f"JMP $$%%X".format(indirectAbsoluteMode())
      /*
        MODE           SYNTAX       HEX LEN TIM
        Absolute      JSR $5597     $20  3   6
      */
      case 0x20 => f"JSR $$%%X".format(absoluteMode())
      /*
        MODE           SYNTAX       HEX LEN TIM
        Immediate     LDA #$44      $A9  2   2
        Zero Page     LDA $44       $A5  2   3
        Zero Page,X   LDA $44,X     $B5  2   4
        Absolute      LDA $4400     $AD  3   4
        Absolute,X    LDA $4400,X   $BD  3   4+
        Absolute,Y    LDA $4400,Y   $B9  3   4+
        Indirect,X    LDA ($44,X)   $A1  2   6
        Indirect,Y    LDA ($44),Y   $B1  2   5+
      */
      case 0xa9 => f"LDA #$$%%02X".format(immediateMode())
      case 0xa5 => f"LDA $$%%X".format(zeroPageMode())
      case 0xb5 => f"LDA $$%%X, X".format(zeroPageIndexedMode(cpu.x))
      case 0xad => f"LDA $$%%X".format(absoluteMode())
      case 0xbd => f"LDA $$%%X, X".format(absoluteIndexedMode(cpu.x))
      case 0xb9 => f"LDA $$%%X, Y".format(absoluteIndexedMode(cpu.y))
      case 0xa1 => f"LDA ($$%%X, X)".format(indexedIndirectMode())
      case 0xb1 => f"LDA ($$%%X), Y".format(indirectIndexedMode())
      /*
        MODE           SYNTAX       HEX LEN TIM
        Immediate     LDX #$44      $A2  2   2
        Zero Page     LDX $44       $A6  2   3
        Zero Page,Y   LDX $44,Y     $B6  2   4
        Absolute      LDX $4400     $AE  3   4
        Absolute,Y    LDX $4400,Y   $BE  3   4+
      */
      case 0xa2 => f"LDX #$$%%02X".format(immediateMode())
      case 0xa6 => f"LDX $$%%X".format(zeroPageMode())
      case 0xb6 => f"LDX $$%%X, X".format(zeroPageIndexedMode(cpu.x))
      case 0xae => f"LDX $$%%X".format(absoluteMode())
      case 0xbe => f"LDX $$%%X, Y".format(absoluteIndexedMode(cpu.y))
      /*
        LDY (LoaD Y register)

        Affects Flags: S Z

        MODE           SYNTAX       HEX LEN TIM
        Immediate     LDY #$44      $A0  2   2
        Zero Page     LDY $44       $A4  2   3
        Zero Page,X   LDY $44,X     $B4  2   4
        Absolute      LDY $4400     $AC  3   4
        Absolute,X    LDY $4400,X   $BC  3   4+
      */
      case 0xa0 => f"LDY #$$%%02X".format(immediateMode())
      case 0xa4 => f"LDY $$%%X".format(zeroPageMode())
      case 0xb4 => f"LDY $$%%X, X".format(zeroPageIndexedMode(cpu.x))
      case 0xac => f"LDY $$%%X".format(absoluteMode())
      case 0xbc => f"LDY $$%%X, X".format(absoluteIndexedMode(cpu.x))
      /*
        LSR (Logical Shift Right)

        Affects Flags: S Z C

        MODE           SYNTAX       HEX LEN TIM
        Accumulator   LSR A         $4A  1   2
        Zero Page     LSR $44       $46  2   5
        Zero Page,X   LSR $44,X     $56  2   6
        Absolute      LSR $4400     $4E  3   6
        Absolute,X    LSR $4400,X   $5E  3   7
      */
      case 0x4a => "LSR A"
      case 0x46 => f"LSR $$%%X".format(zeroPageMode())
      case 0x56 => f"LSR $$%%X, X".format(zeroPageIndexedMode(cpu.x))
      case 0x4e => f"LSR $$%%X".format(absoluteMode())
      case 0x5e => f"LSR $$%%X, X".format(absoluteIndexedMode(cpu.x))
      /*
        NOP (No OPeration)

        Affects Flags: none

        MODE           SYNTAX       HEX LEN TIM
        Implied       NOP           $EA  1   2
      */
      case 0xea => "NOP"
      /*
        ORA (bitwise OR with Accumulator)

        Affects Flags: S Z

        MODE           SYNTAX       HEX LEN TIM
        Immediate     ORA #$44      $09  2   2
        Zero Page     ORA $44       $05  2   2
        Zero Page,X   ORA $44,X     $15  2   3
        Absolute      ORA $4400     $0D  3   4
        Absolute,X    ORA $4400,X   $1D  3   4+
        Absolute,Y    ORA $4400,Y   $19  3   4+
        Indirect,X    ORA ($44,X)   $01  2   6
        Indirect,Y    ORA ($44),Y   $11  2   5+
      */
      case 0x09 => f"ORA #$$%%02X".format(immediateMode())
      case 0x05 => f"ORA $$%%X".format(zeroPageMode())
      case 0x15 => f"ORA $$%%X, X".format(zeroPageIndexedMode(cpu.x))
      case 0x0d => f"ORA $$%%X".format(absoluteMode())
      case 0x1d => f"ORA $$%%X, X".format(absoluteIndexedMode(cpu.x))
      case 0x19 => f"ORA $$%%X, Y".format(absoluteIndexedMode(cpu.y))
      case 0x01 => f"ORA ($$%%X, X)".format(indexedIndirectMode())
      case 0x11 => f"ORA ($$%%X), Y".format(indirectIndexedMode())
      /*
        Stack Instructions

        These instructions are implied mode, have a length of one byte and require machine cycles as indicated. The "PuLl" operations are known as "POP" on most other microprocessors. With the 6502, the stack is always on page one ($100-$1FF) and works top down.

        MNEMONIC                        HEX TIM
        TXS (Transfer X to Stack ptr)   $9A  2
        TSX (Transfer Stack ptr to X)   $BA  2
        PHA (PusH Accumulator)          $48  3
        PLA (PuLl Accumulator)          $68  4
        PHP (PusH Processor status)     $08  3
        PLP (PuLl Processor status)     $28  4
      */
      case 0x9a => "TXS"
      case 0xba => "TSX"
      case 0x48 => "PHA"
      case 0x68 => "PLA"
      case 0x08 => "PHP"
      case 0x28 => "PLP"
      /*
        ROL (ROtate Left)

        Affects Flags: S Z C

        MODE           SYNTAX       HEX LEN TIM
        Accumulator   ROL A         $2A  1   2
        Zero Page     ROL $44       $26  2   5
        Zero Page,X   ROL $44,X     $36  2   6
        Absolute      ROL $4400     $2E  3   6
        Absolute,X    ROL $4400,X   $3E  3   7
      */
      case 0x2a => f"ROL A"
      case 0x26 => f"ROL $$%%X".format(zeroPageMode())
      case 0x36 => f"ROL $$%%X, X".format(zeroPageIndexedMode(cpu.x))
      case 0x2e => f"ROL $$%%X".format(absoluteMode())
      case 0x3e => f"ROL $$%%X, X".format(absoluteIndexedMode(cpu.x))
      /*
        ROR (ROtate Right)

        Affects Flags: S Z C

        MODE           SYNTAX       HEX LEN TIM
        Accumulator   ROR A         $6A  1   2
        Zero Page     ROR $44       $66  2   5
        Zero Page,X   ROR $44,X     $76  2   6
        Absolute      ROR $4400     $6E  3   6
        Absolute,X    ROR $4400,X   $7E  3   7
       */
      case 0x6a => "ROR A"
      case 0x66 => f"ROR $$%%X".format(zeroPageMode())
      case 0x76 => f"ROR $$%%X, X".format(zeroPageIndexedMode(cpu.x))
      case 0x6e => f"ROR $$%%X".format(absoluteMode())
      case 0x7e => f"ROR $$%%X, X".format(absoluteIndexedMode(cpu.x))
      /*
        RTI (ReTurn from Interrupt)

        Affects Flags: all

        MODE           SYNTAX       HEX LEN TIM
        Implied       RTI           $40  1   6
      */
      case 0x40 => "          RTI"
      /*
        RTS (ReTurn from Subroutine)

        Affects Flags: none

        MODE           SYNTAX       HEX LEN TIM
        Implied       RTS           $60  1   6
      */
      case 0x60 => "RTS"
      /*
        SBC (SuBtract with Carry)

        Affects Flags: S V Z C

        MODE           SYNTAX       HEX LEN TIM
        Immediate     SBC #$44      $E9  2   2
        Zero Page     SBC $44       $E5  2   3
        Zero Page,X   SBC $44,X     $F5  2   4
        Absolute      SBC $4400     $ED  3   4
        Absolute,X    SBC $4400,X   $FD  3   4+
        Absolute,Y    SBC $4400,Y   $F9  3   4+
        Indirect,X    SBC ($44,X)   $E1  2   6
        Indirect,Y    SBC ($44),Y   $F1  2   5+
      */
      case 0xe9 => f"SBC #$$%%02X".format(immediateMode())
      case 0xe5 => f"SBC $$%%X".format(zeroPageMode())
      case 0xf5 => f"SBC $$%%X, X".format(zeroPageIndexedMode(cpu.x))
      case 0xed => f"SBC $$%%X".format(absoluteMode())
      case 0xfd => f"SBC $$%%X, X".format(absoluteIndexedMode(cpu.x))
      case 0xf9 => f"SBC $$%%X, Y".format(absoluteIndexedMode(cpu.y))
      case 0xe1 => f"SBC ($$%%X, X)".format(indexedIndirectMode())
      case 0xf1 => f"SBC ($$%%X), Y".format(indirectIndexedMode())
      /*
        STA (STore Accumulator)

        Affects Flags: none

        MODE           SYNTAX       HEX LEN TIM
        Zero Page     STA $44       $85  2   3
        Zero Page,X   STA $44,X     $95  2   4
        Absolute      STA $4400     $8D  3   4
        Absolute,X    STA $4400,X   $9D  3   5
        Absolute,Y    STA $4400,Y   $99  3   5
        Indirect,X    STA ($44,X)   $81  2   6
        Indirect,Y    STA ($44),Y   $91  2   6
      */
      case 0x85 => f"STA $$%%X".format(zeroPageMode())
      case 0x95 => f"STA $$%%X, X".format(zeroPageIndexedMode(cpu.x))
      case 0x8d => f"STA $$%%X".format(absoluteMode())
      case 0x9d => f"STA $$%%X, X".format(absoluteIndexedMode(cpu.x))
      case 0x99 => f"STA $$%%X, Y".format(absoluteIndexedMode(cpu.y))
      case 0x81 => f"STA ($$%%X, X)".format(indexedIndirectMode())
      case 0x91 => f"STA ($$%%X), Y".format(indirectIndexedMode())
      /*
        STX (STore X register)

        Affects Flags: none

        MODE           SYNTAX       HEX LEN TIM
        Zero Page     STX $44       $86  2   3
        Zero Page,Y   STX $44,Y     $96  2   4
        Absolute      STX $4400     $8E  3   4
      */
      case 0x86 => f"STX $$%%X".format(zeroPageMode())
      case 0x96 => f"STX $$%%X, X".format(zeroPageIndexedMode(cpu.x))
      case 0x8e => f"STX $$%%X".format(absoluteMode())
      /*
        STY (STore Y register)

        Affects Flags: none

        MODE           SYNTAX       HEX LEN TIM
        Zero Page     STY $44       $84  2   3
        Zero Page,X   STY $44,X     $94  2   4
        Absolute      STY $4400     $8C  3   4
      */
      case 0x84 => f"STY $$%%X".format(zeroPageMode())
      case 0x94 => f"STY $$%%X, X".format(zeroPageIndexedMode(cpu.x))
      case 0x8c => f"STY $$%%X".format(absoluteMode())
    }

    (if (readBytesLatch) readBytes else "        ") + "  " + "%-32s".format(opcodeString)
  }

  def dump(rom: Array[Byte], file: Option[String]): Unit = {
    require(rom != null)

    val out = file match {
      case None => System.out
      case Some(name) => new FileOutputStream(new File(name))
    }

  }

}

package nesscala.rom

import java.io.{File, FileOutputStream}

import nesscala.nes.{M, Cpu}
import nesscala.util.{IntUtils, BitUtils}

/**
 * Created by chenyan on 15-5-31.
 */
object Disassembler {

  val cpu = new Cpu

  var pc = 0

  def immediateMode(): Int =
    M.ram.read(M.cpu.pc - 1)

  def absoluteMode(): Int =
    BitUtils.makeWord(M.ram.read(pc).toByte, M.ram.read(pc + 1).toByte)

  def zeroPageMode(): Int =
    M.ram.read(pc)

  def indirectAbsoluteMode(): Int = {
    val ret = BitUtils.makeWord(M.ram.read(pc).toByte, M.ram.read(pc + 1).toByte)
    pc += 1
    ret
  }

  def absoluteIndexedMode(index: Int): Int =
    BitUtils.makeWord(M.ram.read(pc).toByte, M.ram.read(pc + 1).toByte) + index

  def zeroPageIndexedMode(index: Int): Int =
    M.ram.read(pc) + index

  def indexedIndirectMode(): Int = {
    val addr = M.ram.read(pc) + IntUtils.toUnsigned(cpu.x)
    BitUtils.makeWord(M.ram.read(addr).toByte, M.ram.read(addr + 1).toByte)
  }

  def indirectIndexedMode(): Int = {
    val addr = M.ram.read(pc)
    BitUtils.makeWord(M.ram.read(addr).toByte, M.ram.read(addr + 1).toByte) + IntUtils.toUnsigned(cpu.y)
  }

  def relativeMode(): Int = 0

  def accumulatorMode(): Int = 0

  def dis(opcode: Int, pc: Int, cpu: Cpu): String = opcode match {
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
    case 0x69 => "ADC 0x%x".format(immediateMode()) // immediate
    case 0x65 => "ADC 0x%x".format(zeroPageMode()) // zeroPage
    case 0x75 => "ADC 0x%x".format(zeroPageIndexedMode(cpu.x))
    case 0x6d => "ADC 0x%x".format(absoluteMode())
    case 0x7d => "ADC 0x%x, X".format(absoluteIndexedMode(cpu.x))
    case 0x79 => "ADC 0x%x, Y".format(absoluteIndexedMode(cpu.y))
    case 0x61 => "ADC (0x%x, X)".format(indexedIndirectMode())
    case 0x71 => "ADC (0x%x), Y".format(indirectIndexedMode())
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
    case 0x29 => "AND 0x%x".format(immediateMode())
    case 0x25 => "AND 0x%x".format(zeroPageMode())
    case 0x35 => "AND 0x%x, X".format(zeroPageIndexedMode(cpu.x))
    case 0x2d => "AND 0x%x".format(absoluteMode())
    case 0x3d => "AND 0x%x, X".format(absoluteIndexedMode(cpu.x))
    case 0x39 => "AND 0x%x, X".format(absoluteIndexedMode(cpu.y))
    case 0x21 => "AND (0x%x, X)".format(indexedIndirectMode())
    case 0x31 => "AND (0x%x), Y".format(indirectIndexedMode())
    /*
      MODE           SYNTAX       HEX LEN TIM
      Accumulator   ASL A         $0A  1   2
      Zero Page     ASL $44       $06  2   5
      Zero Page,X   ASL $44,X     $16  2   6
      Absolute      ASL $4400     $0E  3   6
      Absolute,X    ASL $4400,X   $1E  3   7
    */
    case 0x0a => "ASL A"
    case 0x06 => "ASL 0x%x".format(zeroPageMode())
    case 0x16 => "ASL 0x%x, X".format(zeroPageIndexedMode(cpu.x))
    case 0x0e => "ASL 0x%x".format(absoluteMode())
    case 0x1e => "ASL 0x%x, X".format(absoluteIndexedMode(cpu.x))
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
    case 0x10 => "BPL 0x%x".format(immediateMode())
    case 0x30 => "BMI 0x%x".format(immediateMode())
    case 0x50 => "BVC 0x%x".format(immediateMode())
    case 0x70 => "BVS 0x%x".format(immediateMode())
    case 0x90 => "BCC 0x%x".format(immediateMode())
    case 0xb0 => "BCS 0x%x".format(immediateMode())
    case 0xd0 => "BNE 0x%x".format(immediateMode())
    case 0xf0 => "BEQ 0x%x".format(immediateMode())
    /*
      MODE           SYNTAX       HEX LEN TIM
      Zero Page     BIT $44       $24  2   3
      Absolute      BIT $4400     $2C  3   4
    */
    case 0x24 => "BIT 0x%x".format(zeroPageMode())
    case 0x2c => "BIT 0x%x".format(absoluteMode())
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
    case 0xc9 => "CMP 0x%x".format(immediateMode())
    case 0xc5 => "CMP 0x%x".format(zeroPageMode())
    case 0xd5 => "CMP 0x%x, X".format(zeroPageIndexedMode(cpu.x))
    case 0xcd => "CMP 0x%x".format(absoluteMode())
    case 0xdd => "CMP 0x%x, X".format(absoluteIndexedMode(cpu.x))
    case 0xd9 => "CMP 0x%x, Y".format(absoluteIndexedMode(cpu.y))
    case 0xc1 => "CMP (0x%x, X)".format(indexedIndirectMode())
    case 0xd1 => "CMP (0x%x), Y".format(indirectIndexedMode())
    /*
      MODE           SYNTAX       HEX LEN TIM
      Immediate     CPX #$44      $E0  2   2
      Zero Page     CPX $44       $E4  2   3
      Absolute      CPX $4400     $EC  3   4
    */
    case 0xe0 => "CPX 0x%x".format(immediateMode())
    case 0xe4 => "CPX 0x%x".format(zeroPageMode())
    case 0xec => "CPX 0x%x".format(absoluteMode())
    /*
      MODE           SYNTAX       HEX LEN TIM
      Immediate     CPY #$44      $C0  2   2
      Zero Page     CPY $44       $C4  2   3
      Absolute      CPY $4400     $CC  3   4
    */
    case 0xc0 => "CPY 0x%x".format(immediateMode())
    case 0xc4 => "CPY 0x%x".format(zeroPageMode())
    case 0xcc => "CPY 0x%x".format(absoluteMode())
    /*
      MODE           SYNTAX       HEX LEN TIM
      Zero Page     DEC $44       $C6  2   5
      Zero Page,X   DEC $44,X     $D6  2   6
      Absolute      DEC $4400     $CE  3   6
      Absolute,X    DEC $4400,X   $DE  3   7
    */
    case 0xc6 => "DEC 0x%x".format(zeroPageMode())
    case 0xd6 => "DEC 0x%x, X".format(zeroPageIndexedMode(cpu.x))
    case 0xce => "DEC 0x%x".format(absoluteMode())
    case 0xde => "DEC 0x%x, X".format(absoluteIndexedMode(cpu.x))
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
    case 0x49 => "EOR 0x%x".format(immediateMode())
    case 0x45 => "EOR 0x%x".format(zeroPageMode())
    case 0x55 => "EOR 0x%x, X".format(zeroPageIndexedMode(cpu.x))
    case 0x4d => "EOR 0x%x".format(absoluteMode())
    case 0x5d => "EOR 0x%x, X".format(absoluteIndexedMode(cpu.x))
    case 0x59 => "EOR 0x%x, Y".format(absoluteIndexedMode(cpu.y))
    case 0x41 => "EOR (0x%x, X)".format(indexedIndirectMode())
    case 0x51 => "EOR (0x%x), Y".format(indirectIndexedMode())
    /*
      MODE           SYNTAX       HEX LEN TIM
      Zero Page     INC $44       $E6  2   5
      Zero Page,X   INC $44,X     $F6  2   6
      Absolute      INC $4400     $EE  3   6
      Absolute,X    INC $4400,X   $FE  3   7
    */
    case 0xe6 => "INC 0x%x".format(zeroPageMode())
    case 0xf6 => "INC 0x%x, X".format(zeroPageIndexedMode(cpu.x))
    case 0xee => "INC 0x%x".format(absoluteMode())
    case 0xfe => "INC 0x%x, X".format(absoluteIndexedMode(cpu.x))
    /*
      MODE           SYNTAX       HEX LEN TIM
      Absolute      JMP $5597     $4C  3   3
      Indirect      JMP ($5597)   $6C  3   5
    */
    case 0x4c => "JMP 0x%x".format(absoluteMode())
    case 0x6c => "JMP 0x%x".format(indirectAbsoluteMode())
    /*
      MODE           SYNTAX       HEX LEN TIM
      Absolute      JSR $5597     $20  3   6
    */
    case 0x20 => "JSR 0x%x".format(absoluteMode())
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
    case 0xa9 => "LDA 0x%x".format(immediateMode())
    case 0xa5 => "LDA 0x%x".format(zeroPageMode())
    case 0xb5 => "LDA 0x%x, X".format(zeroPageIndexedMode(cpu.x))
    case 0xad => "LDA 0x%x".format(absoluteMode())
    case 0xbd => "LDA 0x%x, X".format(absoluteIndexedMode(cpu.x))
    case 0xb9 => "LDA 0x%x, Y".format(absoluteIndexedMode(cpu.y))
    case 0xa1 => "LDA (0x%x, X)".format(indexedIndirectMode())
    case 0xb1 => "LDA (0x%x), Y".format(indirectIndexedMode())
    /*
      MODE           SYNTAX       HEX LEN TIM
      Immediate     LDX #$44      $A2  2   2
      Zero Page     LDX $44       $A6  2   3
      Zero Page,Y   LDX $44,Y     $B6  2   4
      Absolute      LDX $4400     $AE  3   4
      Absolute,Y    LDX $4400,Y   $BE  3   4+
    */
    case 0xa2 => "LDX 0x%x".format(immediateMode())
    case 0xa6 => "LDX 0x%x".format(zeroPageMode())
    case 0xb6 => "LDX 0x%x, X".format(zeroPageIndexedMode(cpu.x))
    case 0xae => "LDX 0x%x".format(absoluteMode())
    case 0xbe => "LDX 0x%x, Y".format(absoluteIndexedMode(cpu.y))
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
    case 0xa0 => "LDY 0x%x".format(immediateMode())
    case 0xa4 => "LDY 0x%x".format(zeroPageMode())
    case 0xb4 => "LDY 0x%x, X".format(zeroPageIndexedMode(cpu.x))
    case 0xac => "LDY 0x%x".format(absoluteMode())
    case 0xbc => "LDY 0x%x, X".format(absoluteIndexedMode(cpu.x))
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
    case 0x46 => "LSR 0x%x".format(zeroPageMode())
    case 0x56 => "LSR 0x%x, X".format(zeroPageIndexedMode(cpu.x))
    case 0x4e => "LSR 0x%x".format(absoluteMode())
    case 0x5e => "LSR 0x%x, X".format(absoluteIndexedMode(cpu.x))
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
    case 0x09 => "ORA 0x%x".format(immediateMode())
    case 0x05 => "ORA 0x%x".format(zeroPageMode())
    case 0x15 => "ORA 0x%x, X".format(zeroPageIndexedMode(cpu.x))
    case 0x0d => "ORA 0x%x".format(absoluteMode())
    case 0x1d => "ORA 0x%x, X".format(absoluteIndexedMode(cpu.x))
    case 0x19 => "ORA 0x%x, Y".format(absoluteIndexedMode(cpu.y))
    case 0x01 => "ORA (0x%x, X)".format(indexedIndirectMode())
    case 0x11 => "ORA (0x%x), Y".format(indirectIndexedMode())
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
    case 0x2a => "ROL A"
    case 0x26 => "ROL 0x%x".format(zeroPageMode())
    case 0x36 => "ROL 0x%x, X".format(zeroPageIndexedMode(cpu.x))
    case 0x2e => "ROL 0x%x".format(absoluteMode())
    case 0x3e => "ROL 0x%x, X".format(absoluteIndexedMode(cpu.x))
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
    case 0x66 => "ROR 0x%x".format(zeroPageMode())
    case 0x76 => "ROR 0x%x, X".format(zeroPageIndexedMode(cpu.x))
    case 0x6e => "ROR 0x%x".format(absoluteMode())
    case 0x7e => "ROR 0x%x, X".format(absoluteIndexedMode(cpu.x))
    /*
      RTI (ReTurn from Interrupt)

      Affects Flags: all

      MODE           SYNTAX       HEX LEN TIM
      Implied       RTI           $40  1   6
    */
    case 0x40 => "RTI"
    /*
      RTS (ReTurn from Subroutine)

      Affects Flags: none

      MODE           SYNTAX       HEX LEN TIM
      Implied       RTS           $60  1   6
    */
    case 0x60 => f"RTS "
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
    case 0xe9 => "SBC 0x%x".format(immediateMode())
    case 0xe5 => "SBC 0x%x".format(zeroPageMode())
    case 0xf5 => "SBC 0x%x, X".format(zeroPageIndexedMode(cpu.x))
    case 0xed => "SBC 0x%x".format(absoluteMode())
    case 0xfd => "SBC 0x%x, X".format(absoluteIndexedMode(cpu.x))
    case 0xf9 => "SBC 0x%x, Y".format(absoluteIndexedMode(cpu.y))
    case 0xe1 => "SBC (0x%x, X)".format(indexedIndirectMode())
    case 0xf1 => "SBC (0x%x), Y".format(indirectIndexedMode())
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
    case 0x85 => "STA 0x%x".format(zeroPageMode())
    case 0x95 => "STA 0x%x, X".format(zeroPageIndexedMode(cpu.x))
    case 0x8d => "STA 0x%x".format(absoluteMode())
    case 0x9d => "STA 0x%x, X".format(absoluteIndexedMode(cpu.x))
    case 0x99 => "STA 0x%x, Y".format(absoluteIndexedMode(cpu.y))
    case 0x81 => "STA (0x%x, X)".format(indexedIndirectMode())
    case 0x91 => "STA (0x%x), Y".format(indirectIndexedMode())
    /*
      STX (STore X register)

      Affects Flags: none

      MODE           SYNTAX       HEX LEN TIM
      Zero Page     STX $44       $86  2   3
      Zero Page,Y   STX $44,Y     $96  2   4
      Absolute      STX $4400     $8E  3   4
    */
    case 0x86 => "STX 0x%x".format(zeroPageMode())
    case 0x96 => "STX 0x%x, X".format(zeroPageIndexedMode(cpu.x))
    case 0x8e => "STX 0x%x".format(absoluteMode())
    /*
      STY (STore Y register)

      Affects Flags: none

      MODE           SYNTAX       HEX LEN TIM
      Zero Page     STY $44       $84  2   3
      Zero Page,X   STY $44,X     $94  2   4
      Absolute      STY $4400     $8C  3   4
    */
    case 0x84 => "STY 0x%x".format(zeroPageMode())
    case 0x94 => "STY 0x%x, X".format(zeroPageIndexedMode(cpu.x))
    case 0x8c => "STY 0x%x".format(absoluteMode())
  }

  def dump(rom: Array[Byte], file: Option[String]): Unit = {
    require(rom != null)

    val out = file match {
      case None => System.out
      case Some(name) => new FileOutputStream(new File(name))
    }

  }

}

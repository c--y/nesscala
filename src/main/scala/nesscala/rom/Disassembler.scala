package nesscala.rom

import java.io.{File, FileOutputStream}

/**
 * Created by chenyan on 15-5-31.
 */
object Disassembler {

  def dis(opcode: Int): String = opcode match {
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
    case 0x69 => f"ADC " // immediate
    case 0x65 => f"ADC " // zeroPage
    case 0x75 => f"ADC "
    case 0x6d => f"ADC "
    case 0x7d => f"ADC "
    case 0x79 => f"ADC "
    case 0x61 => f"ADC "
    case 0x71 => f"ADC "
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
    case 0x29 => f"AND "
    case 0x25 => f"AND "
    case 0x35 => f"AND "
    case 0x2d => f"AND "
    case 0x3d => f"AND "
    case 0x39 => f"AND "
    case 0x21 => f"AND "
    case 0x31 => f"AND "
    /*
      MODE           SYNTAX       HEX LEN TIM
      Accumulator   ASL A         $0A  1   2
      Zero Page     ASL $44       $06  2   5
      Zero Page,X   ASL $44,X     $16  2   6
      Absolute      ASL $4400     $0E  3   6
      Absolute,X    ASL $4400,X   $1E  3   7
    */
    case 0x0a => f"ASL "
    case 0x06 => f"ASL "
    case 0x16 => f"ASL "
    case 0x0e => f"ASL "
    case 0x1e => f"ASL "
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
    case 0x10 => f"BPL "
    case 0x30 => f"BMI "
    case 0x50 => f"BVC "
    case 0x70 => f"BVS "
    case 0x90 => f"BCC "
    case 0xb0 => f"BCS "
    case 0xd0 => f"BNE "
    case 0xf0 => f"BEQ "
    /*
      MODE           SYNTAX       HEX LEN TIM
      Zero Page     BIT $44       $24  2   3
      Absolute      BIT $4400     $2C  3   4
    */
    case 0x24 => f"BIT "
    case 0x2c => f"BIT "
    /*
      MODE           SYNTAX       HEX LEN TIM
      Implied       BRK           $00  1   7
    */
    case 0x00 => f"BRK "
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
    case 0x18 => f"CLC "
    case 0x38 => f"SEC "
    case 0x58 => f"CLI "
    case 0x78 => f"SEI "
    case 0xb8 => f"CLV "
    case 0xd8 => f"CLD "
    case 0xf8 => f"SED "
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
    case 0xc9 => f"CMP "
    case 0xc5 => f"CMP "
    case 0xd5 => f"CMP "
    case 0xcd => f"CMP "
    case 0xdd => f"CMP "
    case 0xd9 => f"CMP "
    case 0xc1 => f"CMP "
    case 0xd1 => f"CMP "
    /*
      MODE           SYNTAX       HEX LEN TIM
      Immediate     CPX #$44      $E0  2   2
      Zero Page     CPX $44       $E4  2   3
      Absolute      CPX $4400     $EC  3   4
    */
    case 0xe0 => f"CPX "
    case 0xe4 => f"CPX "
    case 0xec => f"CPX "
    /*
      MODE           SYNTAX       HEX LEN TIM
      Immediate     CPY #$44      $C0  2   2
      Zero Page     CPY $44       $C4  2   3
      Absolute      CPY $4400     $CC  3   4
    */
    case 0xc0 => f"CPY "
    case 0xc4 => f"CPY "
    case 0xcc => f"CPY "
    /*
      MODE           SYNTAX       HEX LEN TIM
      Zero Page     DEC $44       $C6  2   5
      Zero Page,X   DEC $44,X     $D6  2   6
      Absolute      DEC $4400     $CE  3   6
      Absolute,X    DEC $4400,X   $DE  3   7
    */
    case 0xc6 => f"DEC "
    case 0xd6 => f"DEC "
    case 0xce => f"DEC "
    case 0xde => f"DEC "
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
    case 0xaa => f"TAX "
    case 0x8a => f"TXA "
    case 0xca => f"DEX "
    case 0xe8 => f"INX "
    case 0xa8 => f"TAY "
    case 0x98 => f"TYA "
    case 0x88 => f"DEY "
    case 0xc8 => f"INY "
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
    case 0x49 => f"EOR "
    case 0x45 => f"EOR "
    case 0x55 => f"EOR "
    case 0x4d => f"EOR "
    case 0x5d => f"EOR "
    case 0x59 => f"EOR "
    case 0x41 => f"EOR "
    case 0x51 => f"EOR "
    /*
      MODE           SYNTAX       HEX LEN TIM
      Zero Page     INC $44       $E6  2   5
      Zero Page,X   INC $44,X     $F6  2   6
      Absolute      INC $4400     $EE  3   6
      Absolute,X    INC $4400,X   $FE  3   7
    */
    case 0xe6 => f"INC "
    case 0xf6 => f"INC "
    case 0xee => f"INC "
    case 0xfe => f"INC "
    /*
      MODE           SYNTAX       HEX LEN TIM
      Absolute      JMP $5597     $4C  3   3
      Indirect      JMP ($5597)   $6C  3   5
    */
    case 0x4c => f"JMP "
    case 0x6c => f"JMP "
    /*
      MODE           SYNTAX       HEX LEN TIM
      Absolute      JSR $5597     $20  3   6
    */
    case 0x20 => f"JSR "
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
    case 0xa9 => f"LDA "
    case 0xa5 => f"LDA "
    case 0xb5 => f"LDA "
    case 0xad => f"LDA "
    case 0xbd => f"LDA "
    case 0xb9 => f"LDA "
    case 0xa1 => f"LDA "
    case 0xb1 => f"LDA "
    /*
      MODE           SYNTAX       HEX LEN TIM
      Immediate     LDX #$44      $A2  2   2
      Zero Page     LDX $44       $A6  2   3
      Zero Page,Y   LDX $44,Y     $B6  2   4
      Absolute      LDX $4400     $AE  3   4
      Absolute,Y    LDX $4400,Y   $BE  3   4+
    */
    case 0xa2 => f"LDA "
    case 0xa6 => f"LDA "
    case 0xb6 => f"LDA "
    case 0xae => f"LDA "
    case 0xbe => f"LDA "
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
    case 0xa0 => f"LDY "
    case 0xa4 => f"LDY "
    case 0xb4 => f"LDY "
    case 0xac => f"LDY "
    case 0xbc => f"LDY "
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
    case 0x4a => f"LSR "
    case 0x46 => f"LSR "
    case 0x56 => f"LSR "
    case 0x4e => f"LSR "
    case 0x5e => f"LSR "
    /*
      NOP (No OPeration)

      Affects Flags: none

      MODE           SYNTAX       HEX LEN TIM
      Implied       NOP           $EA  1   2
    */
    case 0xea => f"NOP "
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
    case 0x09 => f"ORA "
    case 0x05 => f"ORA "
    case 0x15 => f"ORA "
    case 0x0d => f"ORA "
    case 0x1d => f"ORA "
    case 0x19 => f"ORA "
    case 0x01 => f"ORA "
    case 0x11 => f"ORA "
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
    case 0x9a => f"TXS "
    case 0xba => f"TSX "
    case 0x48 => f"PHA "
    case 0x68 => f"PLA "
    case 0x08 => f"PHP "
    case 0x28 => f"PLP "
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
    case 0x2a => f"ROL "
    case 0x26 => f"ROL "
    case 0x36 => f"ROL "
    case 0x2e => f"ROL "
    case 0x3e => f"ROL "
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
    case 0x6a => f"ROR "
    case 0x66 => f"ROR "
    case 0x76 => f"ROR "
    case 0x6e => f"ROR "
    case 0x7e => f"ROR "
    /*
      RTI (ReTurn from Interrupt)

      Affects Flags: all

      MODE           SYNTAX       HEX LEN TIM
      Implied       RTI           $40  1   6
    */
    case 0x40 => f"RTI "
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
    case 0xe9 => f"SBC "
    case 0xe5 => f"SBC "
    case 0xf5 => f"SBC "
    case 0xed => f"SBC "
    case 0xfd => f"SBC "
    case 0xf9 => f"SBC "
    case 0xe1 => f"SBC "
    case 0xf1 => f"SBC "
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
    case 0x85 => f"STA "
    case 0x95 => f"STA "
    case 0x8d => f"STA "
    case 0x9d => f"STA "
    case 0x99 => f"STA "
    case 0x81 => f"STA "
    case 0x91 => f"STA "
    /*
      STX (STore X register)

      Affects Flags: none

      MODE           SYNTAX       HEX LEN TIM
      Zero Page     STX $44       $86  2   3
      Zero Page,Y   STX $44,Y     $96  2   4
      Absolute      STX $4400     $8E  3   4
    */
    case 0x86 => f"STX "
    case 0x96 => f"STX "
    case 0x8e => f"STX "
    /*
      STY (STore Y register)

      Affects Flags: none

      MODE           SYNTAX       HEX LEN TIM
      Zero Page     STY $44       $84  2   3
      Zero Page,X   STY $44,X     $94  2   4
      Absolute      STY $4400     $8C  3   4
    */
    case 0x84 => f"STY "
    case 0x94 => f"STY "
    case 0x8c => f"STY "
  }

  def dump(rom: Array[Byte], file: Option[String]): Unit = {
    require(rom != null)

    val out = file match {
      case None => System.out
      case Some(name) => new FileOutputStream(new File(name))
    }

  }

}

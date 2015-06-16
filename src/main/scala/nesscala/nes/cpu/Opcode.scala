package nesscala.nes.cpu

import nesscala.nes.Cpu
import nesscala.rom.Disassembler
import nesscala.util.IntUtils

/**
 * Created by chenyan on 15-6-16.
 */
class Opcode(val cpu: Cpu,
             val fn0: Function0[Unit],
             val fn1: Function1[Int, Unit],
             val cycles: Long,
             val addrFn0: Function0[Int],
             val addrFn1: Function1[Byte, Int],
             var addrWay: Symbol) extends Function0[Unit]{

  def evalAddressParam(): Byte = addrWay match {
    case 'X => cpu.x
    case 'Y => cpu.y
    case _ => 0
  }

  override def apply(): Unit = {
    require(fn0 != null || fn1 != null)
    cpu.cycles = cycles
    val address =
      if (addrFn0 != null) addrFn0()
      else if(addrFn1 != null) addrFn1(evalAddressParam())
      else 0
    if (fn0 != null) fn0() else fn1(address)
  }
}

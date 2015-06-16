package nesscala.nes

import java.nio.file.Path

import nesscala.rom.{NesFileLoader, Mapper}

/**
 * Created by chenyan on 15-5-31.
 */
object M {

  val ram: Memory = new Memory

  val cpu: Cpu = new Cpu

  val apu: Apu = new Apu

  var rom: Mapper = null

  val ppu: Ppu = new Ppu()

  var totalCycles: Long = 0

  def loadRom(path: Path): Unit = {
    require(path != null)
    val nesFile = NesFileLoader.readFromFile(path)
    rom = nesFile.getMapper()
    if (nesFile.romControl.hvMirror) {
      ppu.nameTables.mirror('Vertical)
    } else {
      ppu.nameTables.mirror('Horizontal)
    }

  }

  def run(): Unit = {
    require(rom != null, "Need rom loaded.")

    cpu.reset()
    cpu.pc = 0xc000

    while(true) {
      totalCycles += cpu.runStep()

      for (i <- 0 until 3) {
        ppu.runStep()
      }
    }
  }

}

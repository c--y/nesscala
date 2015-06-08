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

  def loadRom(path: Path): Unit = {
    require(path != null)
    val nesFile = NesFileLoader.readFromFile(path)
    rom = nesFile.getMapper()
  }

}

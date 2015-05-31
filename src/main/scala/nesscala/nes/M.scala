package nesscala.nes

import java.nio.file.Path

import nesscala.rom.{NesFileLoader, Mapper}

/**
 * Created by chenyan on 15-5-31.
 */
object M {

  val MemorySize = 0x100000

  val ram: Memory = new Memory(MemorySize)

  val cpu: Cpu = new Cpu(ram)

  val ppu: Ppu = new Ppu

  val apu: Apu = new Apu

  var rom: Mapper = null

  def loadRom(path: Path): Unit = {
    require(path != null)
    val nesFile = NesFileLoader.readFromFile(path)
    rom = nesFile.getMapper()
  }

}

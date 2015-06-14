package nesscala.rom.mapper

import java.nio.file.{Paths, Files}

import nesscala.rom.NesFileLoader
import org.scalatest.FlatSpec

/**
 * Nrom test
 *
 * Created by chenyan on 15-6-14.
 */
class NromSpec extends FlatSpec {

  "A Nrom file" should "loaded from a binary file" in {
    val rom = Files.readAllBytes(
      Paths.get(getClass.getResource("/test_roms/scanline_scanline.nes").getPath))

    val nesFile = NesFileLoader.read(rom)
    val nrom = nesFile.getMapper()
    println(nrom.getType())
    println(nrom)
  }
}

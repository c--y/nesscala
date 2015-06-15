package nesscala.nes

import org.scalatest.FlatSpec

/**
 * Created by chenyan06 on 2015/6/5.
 */
class PpuSpec extends FlatSpec {

  "Ascii graph" should "be drew on console" in {
    val ppu = new Ppu()
    val bytes = Array(
        0x10, 0x00, 0x44, 0x00, 0xfe, 0x00, 0x82,
        0x00, 0x00, 0x28, 0x44, 0x82, 0x00, 0x82, 0x82, 0x00).map(_.toByte)

    println(ppu.dumpTile(bytes))
  }



}

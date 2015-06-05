package nesscala.nes.ppu

import org.scalatest.FlatSpec
import nesscala.nes.ppu.PatternTable

/**
 * Created by chenyan06 on 2015/6/5.
 */
class PatternTableSpec extends FlatSpec {

  "Ascii graph" should "be drew on console" in {
    val pt = new PatternTable
    val bytes = Array(
        0x10, 0x00, 0x44, 0x00, 0xfe, 0x00, 0x82,
        0x00, 0x00, 0x28, 0x44, 0x82, 0x00, 0x82, 0x82, 0x00).map(_.toByte)

    println(pt.dumpTile(bytes))
  }

}

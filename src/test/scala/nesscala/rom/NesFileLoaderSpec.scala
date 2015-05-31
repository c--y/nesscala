package nesscala.rom

import org.scalatest.FlatSpec
import java.nio.file.{Files, Paths}

/**
 *
 *
 * Created by chenyan on 15-5-31.
 */
class NesFileLoaderSpec extends FlatSpec {

   "A nes file object" should "be read from a .nes file." in {
     val rom = Files.readAllBytes(
       Paths.get(getClass.getResource("/test_roms/nesstress.nes").getPath))

     val nesFile = NesFileLoader.read(rom)
     println(nesFile.romControl.toString)
     println(nesFile.prgBanks.length)
     println(nesFile.chrBanks.length)
   }

}

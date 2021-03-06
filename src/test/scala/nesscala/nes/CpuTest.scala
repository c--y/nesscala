package nesscala.nes

import java.nio.file.Paths

/**
 * Created by chenyan on 15-6-15.
 */
object CpuTest {

  val path = Paths.get(getClass.getResource("/test_roms/nestest.nes").getPath)
  M.loadRom(path)

  def main(args: Array[String]): Unit = {
    M.run()
  }
}

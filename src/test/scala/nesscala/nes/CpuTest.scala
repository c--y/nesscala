package nesscala.nes

import java.nio.file.Paths

/**
 * Created by chenyan on 15-6-15.
 */
object CpuTest {

  val path = Paths.get(getClass.getResource("/test_roms/blargg_cpu/all_instrs.nes").getPath)
  M.loadRom(path)

  def main(args: Array[String]): Unit = {
    M.run()
  }
}

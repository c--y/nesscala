package nesscala.util

import org.scalatest.FlatSpec

/**
 * Created by chenyan on 15-5-31.
 */
class IntUtilsSpec extends FlatSpec {

  "a negative short" should "be positive in int type" in {
    val s: Short = -3
    println(s.toBinaryString)
    val us: Int = IntUtils.toUnsigned(s)
    println(s"short(-3) = $us")
    assert(us == 32770)

    // 截断
    val i: Int = 1048575
    println(i.toShort)
    println(i.toShort.toBinaryString)

    val r: Int = i + s
    println(r)

  }

}

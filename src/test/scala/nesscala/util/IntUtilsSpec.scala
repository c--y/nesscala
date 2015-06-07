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

    println((us - 1).toShort)
    println((us - 1).toBinaryString)

    val b1: Byte = 0
    val b2: Byte = 1
    println((b1 - b2))
    println((b1 - b2).toBinaryString)
    println((b1 - b2).toHexString)

    var ib: Byte = -3
    println(ib)
    println(IntUtils.toUnsigned(ib))
  }

}

package nesscala.nes.ppu

import nesscala.util.BitUtils

/**
 * 3 2 1 0
 * X A I Y
 *
 * Created by chenyan on 15-6-3.
 */
class SpriteAttribute(val y: Byte,
                      val index: Byte,
                      val attribute: Byte,
                      val x: Byte) {

  def upperColor2Bits(): Int =
    attribute & 0x03

  def backgroundPrior() =
    BitUtils.isSet(attribute, 2)

  def horizontalFlip() =
    BitUtils.isSet(attribute, 3)

  def verticalFlip() =
    BitUtils.isSet(attribute, 4)

}

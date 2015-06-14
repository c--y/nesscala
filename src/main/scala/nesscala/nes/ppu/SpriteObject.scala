package nesscala.nes.ppu

import nesscala.util.BitUtils

/**
 * 3 2 1 0
 * X A I Y
 *
 * Created by chenyan on 15-6-3.
 */
class SpriteObject {

  var y: Byte = 0
  // Tile index
  var index: Byte = 0
  var attribute: Byte = 0
  var x: Byte = 0

  def upperColor2Bits(): Int =
    attribute & 0x03

  def backgroundPrior() =
    BitUtils.isSet(attribute, 5)

  def horizontalFlip() =
    BitUtils.isSet(attribute, 6)

  def verticalFlip() =
    BitUtils.isSet(attribute, 7)

}

package nesscala.nes.ppu

import nesscala.util.BitUtils

/**
 * Mask Register
 *
 * 7 6 5 4 3 2 1 0
 * B G R s b M m G
 *
 * 7, B, Emphasize blue
 * 6, G, Emphasize green
 * 5, R, Emphasize red
 * 4, s, Show sprites(1:show)
 * 3, b, Show background(1:show)
 * 2, M, 1: Show sprites in leftmost 8 pixels of screen, 0: Hide
 * 1, m, 1: Show backgound in leftmost 8 pixels of screen, 0: Hide
 * 0, G, 1: Produce a grey scale display, 0: normal color
 *
 * Created by chenyan on 15-6-2.
 */
class MaskRegister {

  var v: Byte = 0

  def grayScale() =
    BitUtils.isSet(v, 0)

  def setGrayScale(value: Boolean) =
    v = BitUtils.setBit(v, 0, value)

  def leftmostBackground() =
    BitUtils.isSet(v, 1)

  def setLeftmostBackground(value: Boolean) =
    v = BitUtils.setBit(v, 1, value)

  def leftmostSprite() =
    BitUtils.isSet(v, 2)

  def setLeftmostSprite(value: Boolean) =
    v = BitUtils.setBit(v, 2, value)

  def background() =
    BitUtils.isSet(v, 3)

  def setBackgound(value: Boolean) =
    v = BitUtils.setBit(v, 3, value)

  def sprite() =
    BitUtils.isSet(v, 4)

  def setSprite(value: Boolean) =
    v = BitUtils.setBit(v, 4, value)

  def emphasizeRed() =
    BitUtils.isSet(v, 5)

  def setEmphasizeRed(value: Boolean) =
    v = BitUtils.setBit(v, 5, value)

  def emphasizeGreen() =
    BitUtils.isSet(v, 6)

  def setEmphasizeGreen(value: Boolean) =
    v = BitUtils.setBit(v, 6, value)

  def emphasizeBlue() =
    BitUtils.isSet(v, 7)

  def setEmphasizeBlue(value: Boolean) =
    v = BitUtils.setBit(v, 7, value)
}

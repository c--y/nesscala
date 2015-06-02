package nesscala.nes.ppu

import nesscala.util.BitUtils

/**
 * PPU Status Register
 *   7  bit  0
 *  ---- ----
 *  VSO. ....
 *  |||| ||||
 *  |||+-++++- Least significant bits previously written into a PPU register
 *  |||        (due to register not being updated for this address)
 *  ||+------- Sprite overflow. The intent was for this flag to be set
 *  ||         whenever more than eight sprites appear on a scanline, but a
 *  ||         hardware bug causes the actual behavior to be more complicated
 *  ||         and generate false positives as well as false negatives; see
 *  ||         PPU sprite evaluation. This flag is set during sprite
 *  ||         evaluation and cleared at dot 1 (the second dot) of the
 *  ||         pre-render line.
 *  |+-------- Sprite 0 Hit.  Set when a nonzero pixel of sprite 0 overlaps
 *  |          a nonzero background pixel; cleared at dot 1 of the pre-render
 *  |          line.  Used for raster timing.
 *  +--------- Vertical blank has started (0: not in vblank; 1: in vblank).
 *             Set at dot 1 of line 241 (the line *after* the post-render
 *             line); cleared after reading $2002 and at dot 1 of the
 *             pre-render line.
 *
 * Created by chenyan on 15-6-2.
 */
class StatusRegister {

  var v: Byte = 0

  def leastBits(): Int =
    v & 0x1f

  def setLeastBits(value: Int) =
    v = ((v & 0xe0) | (value & 0x1f)).toByte

  def spriteOverflow() =
    BitUtils.isSet(v, 5)

  def setSpriteOverflow(value: Boolean) =
    v = BitUtils.setBit(v, 5, value)

  def sprite0Hit() =
    BitUtils.isSet(v, 6)

  def setSprite0Hit(value: Boolean) =
    v = BitUtils.setBit(v, 6, value)

  def inVblank() =
    BitUtils.isSet(v, 7)

  def setInVblank(value: Boolean) =
    v = BitUtils.setBit(v, 7, value)

}

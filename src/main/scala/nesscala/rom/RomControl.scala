package nesscala.rom

/**
 * hvMirror: false is Horizontal, true is vertical
 *
 * Created by chenyan on 15-5-30.
 */
class RomControl(
                  val hvMirror: Boolean,
                  val sramEnabled: Boolean,
                  val hasTrainer: Boolean,
                  val fourScreenMirror: Boolean,
                  val mapperType: Byte
                  ) {

  override def toString() =
    s"RomControl{hvMirror=$hvMirror,sramEnabled=$sramEnabled,hasTrainer=$hasTrainer,fourScreenMirror=$fourScreenMirror,mapperType=$mapperType}"
}



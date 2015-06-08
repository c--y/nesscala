package nesscala.nes.cpu

/**
 * Interrupts
 *
 * Created by chenyan on 15-6-8.
 */
class Interrupt
case object InterruptNone extends Interrupt
case object InterruptNmi extends Interrupt
case object InterruptIrq extends Interrupt
case object InterruptReset extends Interrupt


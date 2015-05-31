package nesscala.rom

import java.nio.file.{Paths, Files, Path}

import nesscala.rom.mapper.{Cnrom, Unrom, Mmc1}
import nesscala.util.BitUtils

/**
 * .nes文件格式加载工具
 *
 * Created by chenyan on 15-5-30.
 */
object NesFileLoader {

  val PrgBankSize = 16384

  val ChrBankSize = 8192

  def readFromFile(path: Path): NesFile = {
    val bytes = Files.readAllBytes(path)
    read(bytes)
  }

  def read(bytes: Array[Byte]): NesFile = {
    var i = 0
    // 0-3 "NES"
    bytes.slice(i, i + 3)
    i += 3
    // 0x1A
    bytes(i)
    i += 1
    // Page count of Rom
    val nPrgBank = bytes(4)
    i += 1
    // Page count of Vrom
    val nChrBank = bytes(5)
    i += 1
    // Control byte
    val ctrlByte0 = bytes(6)
    i += 1
    // Upper 4 bits of Mapper
    val ctrlByte1 = bytes(7)
    i += 1
    // Reversed bytes 8 - 15
    bytes.slice(i, i + 8)
    i += 8

    val romControl = parseRomControl(ctrlByte0, ctrlByte1)
    // >= 16, Rom Banks, in ascending order.
    val trainerOffset = if (romControl.hasTrainer) 512 else 0
    val trainer = bytes.slice(i, i + trainerOffset)
    i += trainerOffset

    // Read PRG-ROM & CHR-ROM
    val prgBanks = Array.ofDim[Byte](nPrgBank, PrgBankSize)
    for (j <- 0 until nPrgBank) {
      prgBanks(j) = bytes.slice(i, i + PrgBankSize)
      i += PrgBankSize
    }
    i += PrgBankSize * nPrgBank
    val chrBanks = Array.ofDim[Byte](nChrBank, ChrBankSize)
    for (j <- 0 until nChrBank) {
      chrBanks(j) = bytes.slice(i, i + ChrBankSize)
      i += ChrBankSize
    }

    // Return value
    new NesFile(romControl, prgBanks, chrBanks, trainer)
  }

  def parseRomControl(byte0: Byte, byte1: Byte): RomControl = {
    val hvMirror = BitUtils.isSet(byte0, 0)
    val sramEnabled = BitUtils.isSet(byte0, 1)
    val trainer = BitUtils.isSet(byte0, 2)
    val fourScreenMirror = BitUtils.isSet(byte0, 3)
    val mapperType: Byte = BitUtils.makeUint8((byte0 & 0xF0).toByte, (byte1 & 0xF0).toByte)

    new RomControl(hvMirror, sramEnabled, trainer, fourScreenMirror, mapperType)
  }


}

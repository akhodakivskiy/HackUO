package kdkvsk.hackuo.network.packets.recvsend

import java.io.DataInputStream
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import com.typesafe.scalalogging.LazyLogging
import kdkvsk.hackuo.network.packets.RecvIgnoredPacket
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser, SendPacket}

abstract class GeneralInfoPacketRecv(val subCommand: Int) extends RecvPacket {
  val id: Int = 0xBF
}

abstract class GeneralInfoPacketSend(val subCommand: Int, val length: Int) extends SendPacket {
  val id: Int = 0xBF
  def serialize(out: ByteBuffer): Unit = {
    out.putShort(length.shortValue())
    out.putShort(subCommand.shortValue())
    subSerialize(out)
  }

  def subSerialize(out: ByteBuffer): Unit
}

abstract class GeneralInfoPacketWithIntSend(payload: Int, subCommand: Int) extends GeneralInfoPacketSend(subCommand, 9) {
  def subSerialize(out: ByteBuffer): Unit = out.putInt(payload)
}

/* Receive packets */

case class GiFastWalkPreventionAddKeyPacket(key: Int) extends GeneralInfoPacketRecv(0x2)
case class GiCloseGenericGumpPacket(dialogId: Int, buttonId: Int) extends GeneralInfoPacketRecv(0x4)
case class GiScreenSizePacket(width: Short, height: Short) extends GeneralInfoPacketRecv(0x5)
case class GiSetMapPacket(mapId: Byte) extends GeneralInfoPacketRecv(0x8)

case class GiPopupEntry(textId: Int, index: Short, flags: Short, hueOpt: Option[Short])
case class GiShowPopupPacket(serial: Int, entries: Seq[GiPopupEntry]) extends GeneralInfoPacketRecv(0x14)
case class GiCloseWindow(windowType: Int, serial: Int) extends GeneralInfoPacketRecv(0x16)
object GiClearWeaponAbilityPacket extends GeneralInfoPacketRecv(0x21)
case class GiDamagePacket(serial: Int, damage: Byte) extends GeneralInfoPacketRecv(0x22)
case class GiAbilityChangePacket(abilityId: Byte, state: Byte) extends GeneralInfoPacketRecv(0x22)

/* Send packets */

case class GiClientLanguagePacket(language: String) extends GeneralInfoPacketSend(0xB, 8) {
  def subSerialize(out: ByteBuffer): Unit = out.put(language.getBytes(StandardCharsets.UTF_8).take(3))
}
case class GiCloseStatusBarPacket(serial: Int) extends GeneralInfoPacketWithIntSend(serial, 0xC)
case class GiRequestPopupPacket(serial: Int) extends GeneralInfoPacketWithIntSend(serial, 0x13)
case class GiSelectPopupEntryPacket(serial: Int, entryIndex: Short) extends GeneralInfoPacketSend(0x15, 11) {
  def subSerialize(out: ByteBuffer): Unit = {
    out.putInt(serial)
    out.putShort(entryIndex)
  }
}
case class GiLockStatPacket(stat: Byte, lockState: Byte) extends GeneralInfoPacketSend(0x1A, 7) {
  def subSerialize(out: ByteBuffer): Unit = {
    out.put(stat)
    out.put(lockState)
  }
}
case class GiCastSpellPacket(spellIndex: Byte) extends GeneralInfoPacketSend(0x1C, 9) {
  def subSerialize(out: ByteBuffer): Unit = {
    out.put(2.byteValue())
    out.put(spellIndex)
  }
}
case class GiUseItemPacket(itemSerial: Int, targetSerial: Int) extends GeneralInfoPacketSend(0x2C, 13) {
  def subSerialize(out: ByteBuffer): Unit = {
    out.putInt(itemSerial)
    out.putInt(targetSerial)
  }
}
case class GiUseSpellPacket(spellId: Short, targetSerial: Int) extends GeneralInfoPacketSend(0x2D, 11) {
  def subSerialize(out: ByteBuffer): Unit = {
    out.putShort(spellId)
    out.putInt(targetSerial)
  }
}
case class GiUseSkillPacket(skillId: Short, targetSerial: Int) extends GeneralInfoPacketSend(0x2E, 11) {
  def subSerialize(out: ByteBuffer): Unit = {
    out.putShort(skillId)
    out.putInt(targetSerial)
  }
}

/* Send/Receive packets */

case class GiFastWalkPreventionInitPacket(keys: List[Int]) extends GeneralInfoPacketRecv(0x1) with SendPacket {
  def length: Int = 37

  def serialize(out: ByteBuffer): Unit = {
    out.putShort(subCommand.shortValue())
    keys.foreach { key =>
      out.putInt(key)
    }
  }
}

object GeneralInfoPacketParser extends RecvPacketParser with LazyLogging {
  val packetId: Int = 0xBF

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val packetSize: Short = data.readShort()
    val subCommand: Short = data.readShort()

    if (packetSize != size) {
      throw new IllegalStateException(s"read $size bytes, but claimed packet length is $packetSize")
    }

    subCommand match {
      case 0x1 => GiFastWalkPreventionInitPacket(List.fill(6)(data.readInt()))
      case 0x2 => GiFastWalkPreventionAddKeyPacket(data.readInt)
      case 0x4 => GiCloseGenericGumpPacket(data.readInt(), data.readInt())
      case 0x5 =>
        try {
          data.skipBytes(2)
          GiScreenSizePacket(data.readShort(), data.readShort())
        } finally {
          data.skipBytes(2)
        }
      case 0x6 => RecvIgnoredPacket(packetId, packetSize, "party system")
      case 0x8 => GiSetMapPacket(data.readByte())
      case 0x14 =>
        data.skipBytes(1)
        val subSubCommand: Byte = data.readByte()
        val serial: Int = data.readInt()
        val numEntries: Byte = data.readByte()
        subSubCommand match {
          case 0x2 =>
            val entries: Array[GiPopupEntry] = Array.fill(numEntries)(null)
            Range(0, numEntries).foreach { i =>
              val textId: Int = data.readInt()
              val index: Short = data.readShort()
              val flags: Short = data.readShort()
              val hueOpt: Option[Short] = if ((flags & 0x20) != 0) Some(data.readShort()) else None
              entries(i) = GiPopupEntry(textId, index, flags, hueOpt)
            }
            GiShowPopupPacket(serial, entries)
          case _ => throw new NotImplementedError(s"can't parse non-KR client style popups")
        }
      case 0x16 =>
        val windowType: Int = data.readInt()
        val serial: Int = data.readInt()

        GiCloseWindow(windowType, serial)
      case 0x17 => RecvIgnoredPacket(packetId, packetSize, "codex of wisdom")
      case 0x18 => RecvIgnoredPacket(packetId, packetSize, "enable map diff")
      case 0x19 => RecvIgnoredPacket(packetId, packetSize, "stat locks")
      case 0x1b => RecvIgnoredPacket(packetId, packetSize, "new spellbook")
      case 0x1d => RecvIgnoredPacket(packetId, packetSize, "house revision state")
      case 0x20 => RecvIgnoredPacket(packetId, packetSize, "custom housing")
      case 0x21 => GiClearWeaponAbilityPacket
      case 0x22 =>
        data.skipBytes(1)
        GiDamagePacket(data.readInt(), data.readByte())
      case 0x25 => GiAbilityChangePacket(data.readByte(), data.readByte())
      case _ =>
        logger.info(f"unrecognized subCommand $subCommand%02x")
        RecvIgnoredPacket(packetId, packetSize, s"unrecognized sub command $subCommand%02x")
    }
  }
}

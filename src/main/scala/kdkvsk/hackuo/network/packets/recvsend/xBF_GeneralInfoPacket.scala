package kdkvsk.hackuo.network.packets.recvsend

import java.io.DataInputStream
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import com.typesafe.scalalogging.LazyLogging
import kdkvsk.hackuo.model.common.Serial
import kdkvsk.hackuo.network.packets.RecvIgnoredPacket
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser, SendPacket}

import scala.collection.mutable

object StatLock extends Enumeration {
  type Type = Value

  val Up = Value(0, "up")
  val Down = Value(1, "down")
  val Lock = Value(2, "lock")
}

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

case class xBF_2_FastWalkPreventionAddKeyPacket(key: Int) extends GeneralInfoPacketRecv(0x2)
case class xBF_4_CloseGenericGumpPacket(dialogId: Int, buttonId: Int) extends GeneralInfoPacketRecv(0x4)
case class xBF_5_ScreenSizePacket(width: Short, height: Short) extends GeneralInfoPacketRecv(0x5)
case class xBF_8_SetMapPacket(mapId: Byte) extends GeneralInfoPacketRecv(0x8)

case class xBF_PopupEntry(textId: Int, index: Short, flags: Short, hueOpt: Option[Short])
case class xBF_14_ShowPopupPacket(serial: Int, entries: Seq[xBF_PopupEntry]) extends GeneralInfoPacketRecv(0x14)
case class xBF_16_CloseWindow(windowType: Int, serial: Int) extends GeneralInfoPacketRecv(0x16)
case class xBF_18_EnableMapDiffPacket(landPatchCounts: Seq[Int], staticPatchCounts: Seq[Int]) extends GeneralInfoPacketRecv(0x18)
case class xBF_19_StatLocksPacket(serial: Serial, strength: StatLock.Type, dexterity: StatLock.Type, intelligence: StatLock.Type) extends GeneralInfoPacketRecv(0x19)
object xBF_21_ClearWeaponAbilityPacket extends GeneralInfoPacketRecv(0x21)
case class xBF_22_DamagePacket(serial: Int, damage: Byte) extends GeneralInfoPacketRecv(0x22)
case class xBF_25_AbilityChangePacket(abilityId: Short, state: Byte) extends GeneralInfoPacketRecv(0x22)

/* Send packets */

case class xBF_B_ClientLanguagePacket(language: String) extends GeneralInfoPacketSend(0xB, 8) {
  def subSerialize(out: ByteBuffer): Unit = out.put(language.getBytes(StandardCharsets.UTF_8).take(3))
}
case class xBF_C_CloseStatusBarPacket(serial: Int) extends GeneralInfoPacketWithIntSend(serial, 0xC)
case class xBF_13_RequestPopupPacket(serial: Int) extends GeneralInfoPacketWithIntSend(serial, 0x13)
case class xBF_15_SelectPopupEntryPacket(serial: Int, entryIndex: Short) extends GeneralInfoPacketSend(0x15, 11) {
  def subSerialize(out: ByteBuffer): Unit = {
    out.putInt(serial)
    out.putShort(entryIndex)
  }
}
case class xBF_1A_LockStatPacket(stat: Byte, lockState: Byte) extends GeneralInfoPacketSend(0x1A, 7) {
  def subSerialize(out: ByteBuffer): Unit = {
    out.put(stat)
    out.put(lockState)
  }
}
case class xBF_1C_CastSpellPacket(spellIndex: Byte) extends GeneralInfoPacketSend(0x1C, 9) {
  def subSerialize(out: ByteBuffer): Unit = {
    out.put(2.byteValue())
    out.put(spellIndex)
  }
}
case class xBF_2C_UseItemPacket(itemSerial: Int, targetSerial: Int) extends GeneralInfoPacketSend(0x2C, 13) {
  def subSerialize(out: ByteBuffer): Unit = {
    out.putInt(itemSerial)
    out.putInt(targetSerial)
  }
}
case class xBF_2D_UseSpellPacket(spellId: Short, targetSerial: Int) extends GeneralInfoPacketSend(0x2D, 11) {
  def subSerialize(out: ByteBuffer): Unit = {
    out.putShort(spellId)
    out.putInt(targetSerial)
  }
}
case class xBF_2E_UseSkillPacket(skillId: Short, targetSerial: Int) extends GeneralInfoPacketSend(0x2E, 11) {
  def subSerialize(out: ByteBuffer): Unit = {
    out.putShort(skillId)
    out.putInt(targetSerial)
  }
}

/* Send/Receive packets */

case class xBF_1_FastWalkPreventionInitPacket(keys: List[Int]) extends GeneralInfoPacketRecv(0x1) with SendPacket {
  def length: Int = 37

  def serialize(out: ByteBuffer): Unit = {
    out.putShort(subCommand.shortValue())
    keys.foreach { key =>
      out.putInt(key)
    }
  }
}

object xBF_GeneralInfoPacketParser extends RecvPacketParser with LazyLogging {
  val packetId: Int = 0xBF

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    val packetSize: Short = data.readShort()
    val subCommand: Short = data.readShort()

    if (packetSize != size) {
      throw new IllegalStateException(s"read $size bytes, but claimed packet length is $packetSize")
    }

    subCommand match {
      case 0x1 => xBF_1_FastWalkPreventionInitPacket(List.fill(6)(data.readInt()))
      case 0x2 => xBF_2_FastWalkPreventionAddKeyPacket(data.readInt)
      case 0x4 => xBF_4_CloseGenericGumpPacket(data.readInt(), data.readInt())
      case 0x5 =>
        try {
          data.skipBytes(2)
          xBF_5_ScreenSizePacket(data.readShort(), data.readShort())
        } finally {
          data.skipBytes(2)
        }
      case 0x6 => RecvIgnoredPacket(packetId, packetSize, "party system")
      case 0x8 => xBF_8_SetMapPacket(data.readByte())
      case 0x14 =>
        data.skipBytes(1)
        val subSubCommand: Byte = data.readByte()
        val serial: Int = data.readInt()
        val numEntries: Byte = data.readByte()
        subSubCommand match {
          case 0x2 =>
            val entries: Array[xBF_PopupEntry] = Array.fill(numEntries)(null)
            Range(0, numEntries).foreach { i =>
              val textId: Int = data.readInt()
              val index: Short = data.readShort()
              val flags: Short = data.readShort()
              val hueOpt: Option[Short] = if ((flags & 0x20) != 0) Some(data.readShort()) else None
              entries(i) = xBF_PopupEntry(textId, index, flags, hueOpt)
            }
            xBF_14_ShowPopupPacket(serial, entries)
          case _ => throw new NotImplementedError(s"can't parse non-KR client style popups")
        }
      case 0x16 =>
        val windowType: Int = data.readInt()
        val serial: Int = data.readInt()

        xBF_16_CloseWindow(windowType, serial)
      case 0x17 => RecvIgnoredPacket(packetId, packetSize, "codex of wisdom")
      case 0x18 =>
        val numMaps: Int = data.readInt()
        val landPatchCounts = Array.fill(numMaps)(0)
        val staticPatchCounts = Array.fill(numMaps)(0)
        Range(0, numMaps).foreach { idx =>
          landPatchCounts(idx) = data.readInt()
          staticPatchCounts(idx) = data.readInt()
        }

        xBF_18_EnableMapDiffPacket(landPatchCounts, staticPatchCounts)
      case 0x19 =>
        data.skipBytes(1)
        val serial: Serial = Serial(data.readInt())
        data.skipBytes(1)
        val flags: Int = data.readByte()
        val strength: StatLock.Type = StatLock.apply(flags & 0x3)
        val dexterity: StatLock.Type = StatLock.apply((flags & 0xC) >>> 2)
        val intelligence: StatLock.Type = StatLock.apply((flags & 0x30) >>> 4)
        xBF_19_StatLocksPacket(serial, strength, dexterity, intelligence)
      case 0x1b => RecvIgnoredPacket(packetId, packetSize, "new spellbook")
      case 0x1d => RecvIgnoredPacket(packetId, packetSize, "house revision state")
      case 0x1e => RecvIgnoredPacket(packetId, packetSize, "house serial")
      case 0x20 => RecvIgnoredPacket(packetId, packetSize, "custom housing")
      case 0x21 => xBF_21_ClearWeaponAbilityPacket
      case 0x22 =>
        data.skipBytes(1)
        xBF_22_DamagePacket(data.readInt(), data.readByte())
      case 0x25 => xBF_25_AbilityChangePacket(data.readShort(), data.readByte())
      case _ =>
        logger.info(f"unrecognized subCommand $subCommand%02x")
        RecvIgnoredPacket(packetId, packetSize, s"unrecognized sub command $subCommand%02x")
    }
  }
}

package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.model.common.Serial
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class MobileStatusType5(maxWeight: Short, race: Byte)
case class MobileStatusType3(statsCap: Short, followers: Byte, followersMax: Byte)
case class MobileStatusType4(fireResist: Short, coldResist: Short, poisonResist: Short, energyResist: Short,
                             luck: Short, damageMin: Short, damageMax: Short, tithingPoints: Int)
case class MobileStatusType6(hitChanceIncrease: Short, swingSpeedIncrease: Short, damageChanceIncrease: Short,
                             lowerReagentCost: Short, hitPointsRegeneration: Short,
                             staminaRegeneration: Short, manaRegeneration: Short,
                             reflectPhysicalDamage: Short, enhancePotions: Short,
                             defenceChanceIncrease: Short, spellDamageIncrease: Short,
                             fasterCastRecovery: Short, fasterCasting: Short, lowerManaCost: Short,
                             strengthIncrease: Short, dexterityIncrease: Short, intelligenceIncrease: Short,
                             hitPointsIncrease: Short, staminaIncrease: Short, manaIncrease: Short,
                             maximumHitPointsIncrease: Short, maximumStaminaIncrease: Short, maximumManaIncrease: Short)

case class MobileStatusPacket(serial: Serial, name: String, hits: Short, maxHits: Short, nameChangeFlag: Byte,
                              statusFlag: Byte, sexAndRace: Byte, strength: Short, dexterity: Short,
                              intelligence: Short, stamina: Short, maxStamina: Short, mana: Short, maxMana: Short,
                              gold: Int, armorRating: Short, weight: Short,
                              type5Opt: Option[MobileStatusType5],
                              type3Opt: Option[MobileStatusType3],
                              type4Opt: Option[MobileStatusType4],
                              type6Opt: Option[MobileStatusType6]) extends RecvPacket {

  def id: Int = MobileStatusPacketParser.packetId

  def maxWeight(default: Int): Int = type5Opt.map(_.maxWeight.toInt).getOrElse(default)
  def followers(default: Int): Int = type3Opt.map(_.followers.toInt).getOrElse(default)
  def followersMax(default: Int): Int = type3Opt.map(_.followersMax.toInt).getOrElse(default)
  def fireResist(default: Int): Int = type4Opt.map(_.fireResist.toInt).getOrElse(default)
  def coldResist(default: Int): Int = type4Opt.map(_.coldResist.toInt).getOrElse(default)
  def poisonResist(default: Int): Int = type4Opt.map(_.poisonResist.toInt).getOrElse(default)
  def energyResist(default: Int): Int = type4Opt.map(_.energyResist.toInt).getOrElse(default)
  def luck(default: Int): Int = type4Opt.map(_.luck.toInt).getOrElse(default)
  def damageMin(default: Int): Int = type4Opt.map(_.damageMin.toInt).getOrElse(default)
  def damageMax(default: Int): Int = type4Opt.map(_.damageMax.toInt).getOrElse(default)
}

object MobileStatusPacketParser extends RecvPacketParser {
  val packetId: Int = 0x11

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    ensureLength(data, size)

    val serial: Serial = Serial(data.readInt())
    val name: String = readStringWithNull(data, 30)
    val hits: Short = data.readShort()
    val maxHits: Short = data.readShort()
    val nameChangeFlag: Byte = data.readByte()
    val statusFlag: Byte = data.readByte()
    val sexAndRace: Byte = data.readByte()
    val strength: Short = data.readShort()
    val dexterity: Short = data.readShort()
    val intelligence: Short = data.readShort()
    val stamina: Short = data.readShort()
    val maxStamina: Short = data.readShort()
    val mana: Short = data.readShort()
    val maxMana: Short = data.readShort()
    val gold: Int = data.readInt()
    val armorRating: Short = data.readShort()
    val weight: Short = data.readShort()

    val mobileStatusType5Opt: Option[MobileStatusType5] = if (statusFlag >= 5) {
      Some(MobileStatusType5(
        maxWeight = data.readShort(),
        race = data.readByte))
    } else {
      None
    }

    val mobileStatusType3Opt: Option[MobileStatusType3] = if (statusFlag >= 3) {
      Some(MobileStatusType3(
        statsCap = data.readShort(),
        followers = data.readByte(),
        followersMax = data.readByte()))
    } else {
      None
    }

    val mobileStatusType4Opt: Option[MobileStatusType4] = if (statusFlag >= 4) {
      Some(MobileStatusType4(
        fireResist = data.readShort(),
        coldResist = data.readShort(),
        poisonResist = data.readShort(),
        energyResist = data.readShort(),
        luck = data.readShort(),
        damageMin = data.readShort(),
        damageMax = data.readShort(),
        tithingPoints = data.readInt()
      ))
    } else {
      None
    }

    /*
    val mobileStatusType6Opt: Option[MobileStatusType6] = if (statusFlag >= 6) {
      Some(MobileStatusType6(
        hitChanceIncrease = data.readShort(),
        swingSpeedIncrease = data.readShort(),
        damageChanceIncrease = data.readShort(),
        lowerReagentCost = data.readShort(),
        hitPointsRegeneration = data.readShort(),
        staminaRegeneration = data.readShort(),
        manaRegeneration = data.readShort(),
        reflectPhysicalDamage = data.readShort(),
        enhancePotions = data.readShort(),
        defenceChanceIncrease = data.readShort(),
        spellDamageIncrease = data.readShort(),
        fasterCastRecovery = data.readShort(),
        fasterCasting = data.readShort(),
        lowerManaCost = data.readShort(),
        strengthIncrease = data.readShort(),
        dexterityIncrease = data.readShort(),
        intelligenceIncrease = data.readShort(),
        hitPointsIncrease = data.readShort(),
        staminaIncrease = data.readShort(),
        manaIncrease = data.readShort(),
        maximumHitPointsIncrease = data.readShort(),
        maximumStaminaIncrease = data.readShort(),
        maximumManaIncrease = data.readShort()
      ))
    } else {
      None
    }
    */

    val result: MobileStatusPacket = MobileStatusPacket(
      serial = serial,
      name = name,
      hits = hits,
      maxHits = maxHits,
      nameChangeFlag = nameChangeFlag,
      statusFlag = statusFlag,
      sexAndRace = sexAndRace,
      strength = strength,
      dexterity = dexterity,
      intelligence = intelligence,
      stamina = stamina,
      maxStamina = maxStamina,
      mana = mana,
      maxMana = maxMana,
      gold = gold,
      armorRating = armorRating,
      weight = weight,
      type5Opt = mobileStatusType5Opt,
      type3Opt = mobileStatusType3Opt,
      type4Opt = mobileStatusType4Opt,
      type6Opt = None
    )

    /*
    if (result.length != packetSize) {
      throw new IllegalStateException(s"expected packet size: $packetSize, actual packet size: ${result.length}")
    }
    */

    result
  }
}

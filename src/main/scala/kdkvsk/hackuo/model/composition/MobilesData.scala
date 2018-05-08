package kdkvsk.hackuo.model.composition
import cats.data.State
import com.typesafe.scalalogging.LazyLogging
import kdkvsk.hackuo.{Message, PacketMessage, Response}
import kdkvsk.hackuo.model.{Mobile, MobileItem, World}
import kdkvsk.hackuo.model.common.{ItemLayer, Serial}
import kdkvsk.hackuo.network.RecvPacket
import kdkvsk.hackuo.network.packets.recv._
import kdkvsk.hackuo.network.packets.recvsend._

case class MobilesData(playerSerial: Serial,
                       mobiles: Map[Serial, Mobile],
                       item2mobile: Map[Serial, Serial]) extends Part[MobilesData] {
  lazy val player: Mobile = mobiles.getOrElse(playerSerial, throw new IllegalStateException(s"player serial not set"))
}

object MobilesData extends PartOps[World, MobilesData] with LazyLogging {
  def get(w: World): MobilesData = w.mobiles
  def set(w: World, t: MobilesData): World = w.copy(mobiles = t)

  val handler: PartialFunction[Message, State[MobilesData, Response]] = {
    case PacketMessage(p: x1B_LoginConfirmPacket) => loginConfirm(p)
    case PacketMessage(p: x20_DrawPlayerPacket) => drawPlayer(p)
    case PacketMessage(p: x78_MobileIncomingPacket) => mobileIncoming(p)
    case PacketMessage(p: x11_MobileStatusPacket) => mobileStatus(p)
    case PacketMessage(p: x2D_UpdateMobileStatusPacket) => updateMobileStatus(p)
    case PacketMessage(p: x77_UpdateMobilePacket) => updateMobile(p)
    case PacketMessage(p: x2E_WearItemByMobilePacket) => wearItemByMobile(p)
    case PacketMessage(p: x72_WarModePacket) => warMode(p)
    case PacketMessage(p: x1D_DeletePacket) => deleteMobile(p)
    case PacketMessage(p: xDC_ItemRevisionHashPacket) => itemRevision(p)
  }

  def loginConfirm(p: x1B_LoginConfirmPacket): State[MobilesData, Unit] = State.modify { m =>
    m.copy(
      playerSerial = p.serial,
      mobiles = m.mobiles.updated(p.serial, Mobile(p.serial, p.typeId, 0, p.x, p.y, p.z, flags = 0, p.direction)))
  }

  def drawPlayer(p: x20_DrawPlayerPacket): State[MobilesData, Unit] = State.modify { m =>
    val mobile: Mobile = m.mobiles
      .getOrElse(p.serial, Mobile(p.serial, p.bodyId, p.hue, p.x, p.y, p.z, p.flags, p.direction))
      .copy(bodyId = p.bodyId, hue = p.hue, x = p.x, y = p.y, z = p.z, direction = p.direction)

    m.copy(mobiles = m.mobiles.updated(mobile.serial, mobile))
  }

  def mobileIncoming(p: x78_MobileIncomingPacket): State[MobilesData, Unit] = State.modify { m =>
    val items: Map[Serial, MobileItem] = p.items.map { i =>
      i.serial -> MobileItem(i.serial, i.graphicId, ItemLayer(i.layer), i.hue, 0)
    }.toMap

    val mobile: Mobile = m.mobiles
      .getOrElse(p.serial, Mobile(p.serial, p.bodyId, p.hue, p.x, p.y, p.z, p.flags, p.direction))
      .copy(bodyId = p.bodyId, hue = p.hue, x = p.x, y = p.y, z = p.z, direction = p.direction, notoriety = p.notoriety, items = items)

    m.copy(mobiles = m.mobiles.updated(mobile.serial, mobile), item2mobile = m.item2mobile ++ p.items.map(i => i.serial -> mobile.serial))
  }

  def mobileStatus(p: x11_MobileStatusPacket): State[MobilesData, Unit] = State.modify { w =>
    w.mobiles.get(p.serial) match {
      case None =>
        logger.warn(s"mobile ${p.serial} not found for ${p.getClass.getSimpleName} update")
        w
      case Some(mobile) =>
        val newMobile: Mobile = mobile.copy(
          name = p.name,
          hits = p.hits,
          maxHits = p.maxHits,
          stamina = p.stamina,
          maxStamina = p.maxStamina,
          mana = p.mana,
          maxMana = p.maxMana,
          strength = p.strength,
          dexterity = p.dexterity,
          intelligence = p.intelligence,
          gold = p.gold,
          weight = p.weight,
          maxWeight = p.maxWeight(mobile.maxWeight),
          followers = p.followers(mobile.followers),
          maxFollowers = p.followersMax(mobile.maxFollowers),
          physicalResist = p.armorRating,
          fireResist = p.fireResist(mobile.fireResist),
          coldResist = p.coldResist(mobile.coldResist),
          poisonResist = p.poisonResist(mobile.poisonResist),
          energyResist = p.energyResist(mobile.energyResist),
          luck = p.luck(mobile.energyResist),
          damageMin = p.damageMin(mobile.damageMin),
          damageMax = p.damageMax(mobile.damageMax)
        )

        w.copy(mobiles = w.mobiles.updated(p.serial, newMobile))
    }
  }

  def updateMobileStatus(p: x2D_UpdateMobileStatusPacket): State[MobilesData, Unit] = State.modify { w =>
    w.mobiles.get(p.serial) match {
      case None =>
        logger.warn(s"mobile ${p.serial} not found for ${p.getClass.getSimpleName} update")
        w
      case Some(mobile) =>
        val newMobile: Mobile = mobile.copy(
          hits = p.hits, maxHits = p.maxHits,
          mana = p.mana, maxMana = p.maxMana,
          stamina = p.stamina, maxStamina = p.maxStamina
        )

        w.copy(mobiles = w.mobiles.updated(p.serial, newMobile))
    }
  }

  def wearItemByMobile(p: x2E_WearItemByMobilePacket): State[MobilesData, Unit] = State.modify { w =>
    w.mobiles.get(p.mobileSerial) match {
      case None =>
        logger.warn(s"mobile ${p.mobileSerial} not found for ${p.getClass.getSimpleName} update")
        w
      case Some(mobile) =>
        val i: MobileItem = MobileItem(p.serial, p.graphicId, ItemLayer(p.layer), p.hue, 0x0)
        w.copy(mobiles = w.mobiles.updated(mobile.serial, mobile.copy(items = mobile.items.updated(i.serial, i))),
          item2mobile = w.item2mobile.updated(i.serial, mobile.serial))
    }
  }

  def warMode(p: x72_WarModePacket): State[MobilesData, Unit] = State.modify { w =>
    w.mobiles.get(w.playerSerial) match {
      case Some(player) => w.copy(mobiles = w.mobiles.updated(player.serial, player.copy(warMode = p.mode == 1)))
      case None => w
    }
  }

  def updateMobile(p: x77_UpdateMobilePacket): State[MobilesData, Unit] = State.modify { w =>
    w.mobiles.get(p.serial) match {
      case Some(mobile) =>
        val newMobile = mobile.copy(bodyId = p.bodyId, x = p.x, y = p.y, z = p.z, direction = p.direction, hue = p.hue, flags = p.flags)
        w.copy(mobiles = w.mobiles.updated(p.serial, newMobile))
      case None =>
        logger.warn(s"mobile ${p.serial} not found for ${p.getClass.getSimpleName} update")
        w
    }
  }

  def deleteMobile(p: x1D_DeletePacket): State[MobilesData, Unit] = State.modify { w =>
    w.mobiles.get(p.serial) match {
      case Some(m) => w.copy(mobiles = w.mobiles - p.serial, item2mobile = w.item2mobile -- m.items.keys)
      case None => w
    }
  }

  def itemRevision(p: xDC_ItemRevisionHashPacket): State[MobilesData, Unit] = State.modify { w =>
    val worldOpt: Option[MobilesData] = for {
      mid <- w.item2mobile.get(p.serial)
      mobile <- w.mobiles.get(mid)
      item <- mobile.items.get(p.serial)
    } yield {
      w.copy(mobiles = w.mobiles.updated(mid, mobile.copy(items = mobile.items.updated(item.serial, item.copy(itemHash = p.itemHash)))))
    }

    worldOpt.getOrElse(w)
  }
}

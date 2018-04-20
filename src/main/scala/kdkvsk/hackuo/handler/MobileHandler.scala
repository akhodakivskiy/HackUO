package kdkvsk.hackuo.handler
import com.typesafe.scalalogging.LazyLogging
import kdkvsk.hackuo.{Message, PacketMessage}
import kdkvsk.hackuo.model._
import kdkvsk.hackuo.model.common.{ItemLayer, Serial}
import kdkvsk.hackuo.network.packets.recv._
import kdkvsk.hackuo.network.packets.recvsend.{GiSetMapPacket, WarModePacket}

object MobileHandler extends Handler with MobileHandlerOps {
  val handle: PartialFunction[Message, World.State] = {
    case PacketMessage(p: LoginConfirmPacket) => loginConfirm(p)
    case PacketMessage(p: DrawPlayerPacket) => drawPlayer(p)
    case PacketMessage(p: MobileIncomingPacket) => mobileIncoming(p)
    case PacketMessage(p: MobileStatusPacket) => mobileStatus(p)
    case PacketMessage(p: WarModePacket) => warMode(p)
    case PacketMessage(p: DeletePacket) => deleteMobile(p)
    case PacketMessage(p: ItemRevisionHashPacket) => itemRevision(p)
  }
}

trait MobileHandlerOps extends LazyLogging {
  def loginConfirm(p: LoginConfirmPacket): World.State = World.modify { w =>
    w.copy(
      playerSerial = p.serial,
      mobiles = w.mobiles.updated(p.serial, Mobile(p.serial, p.typeId, 0, p.x, p.y, p.z, flags = 0, p.direction)))
  }

  def drawPlayer(p: DrawPlayerPacket): World.State = World.modify { w =>
    val mobile: Mobile = w.mobiles
      .getOrElse(p.serial, Mobile(p.serial, p.bodyId, p.hue, p.x, p.y, p.z, p.flags, p.direction))
      .copy(bodyId = p.bodyId, hue = p.hue, x = p.x, y = p.y, z = p.z, direction = p.direction)

    w.copy(mobiles = w.mobiles.updated(mobile.serial, mobile))
  }

  def mobileIncoming(p: MobileIncomingPacket): World.State = World.modify { w =>
    val items: Map[Serial, MobileItem] = p.items.map { i =>
      i.serial -> MobileItem(i.serial, i.graphicId, ItemLayer(i.layer), i.hue, 0)
    }.toMap

    val mobile: Mobile = w.mobiles
      .getOrElse(p.serial, Mobile(p.serial, p.bodyId, p.hue, p.x, p.y, p.z, p.flags, p.direction))
      .copy(bodyId = p.bodyId, hue = p.hue, x = p.x, y = p.y, z = p.z, direction = p.direction, notoriety = p.notoriety, items = items)

    w.copy(mobiles = w.mobiles.updated(mobile.serial, mobile), item2mobile = w.item2mobile ++ p.items.map(i => i.serial -> mobile.serial))
  }

  def mobileStatus(p: MobileStatusPacket): World.State = World.modify { w =>
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

  def warMode(p: WarModePacket): World.State = World.modify { w =>
    w.mobiles.get(w.playerSerial) match {
      case Some(player) => w.copy(mobiles = w.mobiles.updated(player.serial, player.copy(warMode = p.mode == 1)))
      case None => w
    }
  }

  def updatePlayer(p: UpdatePlayerPacket): World.State = World.modify { w =>
    w.mobiles.get(p.serial) match {
      case Some(mobile) =>
        val newMobile = mobile.copy(bodyId = p.bodyId, x = p.x, y = p.y, z = p.z, direction = p.direction, hue = p.hue, flags = p.flags)
        w.copy(mobiles = w.mobiles.updated(p.serial, newMobile))
      case None =>
        logger.warn(s"mobile ${p.serial} not found for ${p.getClass.getSimpleName} update")
        w
    }
  }

  def deleteMobile(p: DeletePacket): World.State = World.modify { w =>
    w.mobiles.get(p.serial) match {
      case Some(m) => w.copy(mobiles = w.mobiles - p.serial, item2mobile = w.item2mobile -- m.items.keys)
      case None => w
    }
  }

  def itemRevision(p: ItemRevisionHashPacket): World.State = World.modify { w =>
    val worldOpt: Option[World] = for {
      mid <- w.item2mobile.get(p.serial)
      mobile <- w.mobiles.get(mid)
      item <- mobile.items.get(p.serial)
    } yield {
      w.copy(mobiles = w.mobiles.updated(mid, mobile.copy(items = mobile.items.updated(item.serial, item.copy(itemHash = p.itemHash)))))
    }

    worldOpt.getOrElse(w)
  }
}

package kdkvsk.hackuo.handler
import com.typesafe.scalalogging.LazyLogging
import kdkvsk.hackuo.model._
import kdkvsk.hackuo.model.common.ItemLayer
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
  }
}

trait MobileHandlerOps extends LazyLogging {
  def loginConfirm(p: LoginConfirmPacket): World.State = World.modify { w =>
    w.copy(
      playerSerial = p.serial,
      mobiles = w.mobiles.updated(p.serial, Mobile(p.serial, p.bodyId, 0, p.x, p.y, p.z, flags = 0, p.direction)))
  }

  def drawPlayer(p: DrawPlayerPacket): World.State = World.modify { w =>
    val mobile: Mobile = w.mobiles
      .getOrElse(p.serial, Mobile(p.serial, p.body, p.hue, p.x, p.y, p.z, p.flags, p.direction))
      .copy(body = p.body, hue = p.hue, x = p.x, y = p.y, z = p.z, direction = p.direction)

    w.copy(mobiles = w.mobiles.updated(mobile.serial, mobile))
  }

  def mobileIncoming(p: MobileIncomingPacket): World.State = World.modify { w =>
    val items: Seq[MobileItem] = p.items.map { i =>
      MobileItem(i.serial, i.itemId, ItemLayer(i.layer), i.hue)
    }

    val mobile: Mobile = w.mobiles
      .getOrElse(p.serial, Mobile(p.serial, p.body, p.hue, p.x, p.y, p.z, p.flags, p.direction))
      .copy(body = p.body, hue = p.hue, x = p.x, y = p.y, z = p.z, direction = p.direction, notoriety = p.notoriety, items = items)

    w.copy(mobiles = w.mobiles.updated(mobile.serial, mobile))
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
        val newMobile = mobile.copy(body = p.body, x = p.x, y = p.y, z = p.z, direction = p.direction, hue = p.hue, flags = p.flags)
        w.copy(mobiles = w.mobiles.updated(p.serial, newMobile))
      case None =>
        logger.warn(s"mobile ${p.serial} not found for ${p.getClass.getSimpleName} update")
        w
    }
  }

  def deleteMobile(p: DeletePacket): World.State = World.modify { w =>
    w.copy(mobiles = w.mobiles - p.serial)
  }
}

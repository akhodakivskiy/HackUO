package kdkvsk.hackuo.handler
import com.typesafe.scalalogging.LazyLogging
import kdkvsk.hackuo.model._
import kdkvsk.hackuo.model.common.{ItemLayer, Serial}
import kdkvsk.hackuo.network.packets.recv._
import kdkvsk.hackuo.{Message, PacketMessage}

object ItemHandler extends Handler with ItemHandlerOps {
  val handle: PartialFunction[Message, World.State] = {
    case PacketMessage(p: ItemInfoPacket) => itemInfo(p)
    case PacketMessage(p: MultiInfoPacket) => multiInfo(p)
    case PacketMessage(p: ItemRevisionHashPacket) => itemHash(p)
    case PacketMessage(p: DeletePacket) => deleteItem(p)
  }
}

trait ItemHandlerOps extends LazyLogging {
  def itemInfo(p: ItemInfoPacket): World.State = World.modify { w =>
    val item: ObjectItem = ObjectItem(p.serial, p.typeId, p.amount, p.x, p.y, p.z, p.light, p.hue, p.direction, p.flag, 0)
    w.copy(objects = w.objects.updated(p.serial, item))
  }

  def multiInfo(p: MultiInfoPacket): World.State = World.modify { w =>
    val item: MultiItem = MultiItem(p.serial, p.graphicId, p.x, p.y, p.z, 0)
    w.copy(multis = w.multis.updated(p.serial, item))
  }

  def itemHash(p: ItemRevisionHashPacket): World.State = World.modify { w =>
    val w1 = w.objects.get(p.serial) match {
      case Some(obj) => w.copy(objects = w.objects.updated(p.serial, obj.copy(itemHash = p.itemHash)))
      case None => w
    }

    w1.multis.get(p.serial) match {
      case Some(mul) => w1.copy(multis = w1.multis.updated(p.serial, mul.copy(itemHash = p.itemHash)))
      case None => w1
    }
  }

  def deleteItem(p: DeletePacket): World.State = World.modify { w =>
    w.copy(objects = w.objects - p.serial, multis = w.multis - p.serial)
  }
}

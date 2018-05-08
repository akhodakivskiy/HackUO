package kdkvsk.hackuo.model.composition

import cats.data.State
import kdkvsk.hackuo.{Message, PacketMessage, Response}
import kdkvsk.hackuo.model.{MultiItem, ObjectItem, World}
import kdkvsk.hackuo.model.common.Serial
import kdkvsk.hackuo.network.packets.recv.{x1D_DeletePacket, xDC_ItemRevisionHashPacket, xF3_ItemInfoPacket, xF3_MultiInfoPacket}
import kdkvsk.hackuo.network.packets.recvsend.xBF_8_SetMapPacket

case class MapData(mapId: Int,
                   objects: Map[Serial, ObjectItem] = Map.empty,
                   multis: Map[Serial, MultiItem] = Map.empty) extends Part[MapData] {

}

object MapData extends PartOps[World, MapData] {
  def get(w: World): MapData = w.map

  def set(w: World, t: MapData): World = w.copy(map = t)

  val handler: PartialFunction[Message, State[MapData, Response]] = {
    case PacketMessage(p: xF3_ItemInfoPacket) => itemInfo(p)
    case PacketMessage(p: xF3_MultiInfoPacket) => multiInfo(p)
    case PacketMessage(p: xDC_ItemRevisionHashPacket) => itemHash(p)
    case PacketMessage(p: x1D_DeletePacket) => deleteItem(p)
    case PacketMessage(p: xBF_8_SetMapPacket) => setMap(p)
  }

  def itemInfo(p: xF3_ItemInfoPacket): State[MapData, Unit] = State.modify { w =>
    val item: ObjectItem = ObjectItem(p.serial, p.typeId, p.amount, p.x, p.y, p.z, p.light, p.hue, p.direction, p.flag, 0)
    w.copy(objects = w.objects.updated(p.serial, item))
  }

  def multiInfo(p: xF3_MultiInfoPacket): State[MapData, Unit] = State.modify { w =>
    val item: MultiItem = MultiItem(p.serial, p.graphicId, p.x, p.y, p.z, 0)
    w.copy(multis = w.multis.updated(p.serial, item))
  }

  def itemHash(p: xDC_ItemRevisionHashPacket): State[MapData, Unit] = State.modify { w =>
    val w1 = w.objects.get(p.serial) match {
      case Some(obj) => w.copy(objects = w.objects.updated(p.serial, obj.copy(itemHash = p.itemHash)))
      case None => w
    }

    w1.multis.get(p.serial) match {
      case Some(mul) => w1.copy(multis = w1.multis.updated(p.serial, mul.copy(itemHash = p.itemHash)))
      case None => w1
    }
  }

  def deleteItem(p: x1D_DeletePacket): State[MapData, Unit] = State.modify { w =>
    w.copy(objects = w.objects - p.serial, multis = w.multis - p.serial)
  }

  def setMap(p: xBF_8_SetMapPacket): cats.data.State[MapData, Unit] = cats.data.State.modify { w =>
    w.copy(mapId = p.mapId)
  }
}

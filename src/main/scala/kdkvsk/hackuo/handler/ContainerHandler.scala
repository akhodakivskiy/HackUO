package kdkvsk.hackuo.handler

import cats.data.State
import com.typesafe.scalalogging.LazyLogging
import kdkvsk.hackuo.model.common.Serial
import kdkvsk.hackuo.{Message, MultiResponse, PacketMessage, Response}
import kdkvsk.hackuo.model.{Container, ContainerItem, World}
import kdkvsk.hackuo.network.packets.recv.{AddContainerItemsPacket, DeletePacket, DrawContainerPacket, ItemRevisionHashPacket}

object ContainerHandler extends Handler with ContainerHandlerOps {
  val handle: PartialFunction[Message, State[World, Response]] = {
    case PacketMessage(p: ItemRevisionHashPacket) => itemHash(p)
    case PacketMessage(p: DrawContainerPacket) => drawContainer(p)
    case PacketMessage(p: AddContainerItemsPacket) => addContainerItems(p)
    case PacketMessage(p: DeletePacket) => deleteItem(p)
  }
}

trait ContainerHandlerOps extends LazyLogging {

  def drawContainer(p: DrawContainerPacket): World.State = World.modify { w =>
    w.copy(containers = w.containers.updated(p.serial, Container(p.serial, p.gumpId, Map.empty)))
  }

  def addContainerItems(p: AddContainerItemsPacket): World.State = World.modify { world =>
    p.items.groupBy(_.containerSerial).foldLeft(world) {
      case (w, (cid, its)) =>
        w.containers.get(cid) match {
          case Some(c) =>
            val newItems: Map[Serial, ContainerItem] = its.map { i =>
              i.serial -> ContainerItem(i.serial, i.graphicId, i.amount,
                i.x, i.y, i.gridIndexOpt.map(_.intValue()),
                i.hue, i.containerSerial, itemHash = 0)
            }.toMap
            val i2c: Map[Serial, Serial] = newItems.values.map(i => i.serial -> i.containerSerial).toMap
            w.copy(containers = w.containers.updated(c.serial, c.copy(items = c.items ++ newItems)), item2container = w.item2container ++ i2c)
          case None =>
            logger.warn(s"container $cid does not exist - while adding items to container")
            w
        }
    }
  }

  def deleteItem(p: DeletePacket): World.State = {
    for {
      p1 <- deleteContainer(p.serial)
      p2 <- deleteContainerItem(p.serial)
    } yield {
      MultiResponse.flatten(p1, p2)
    }
  }

  def deleteContainer(serial: Serial): World.State = World.modify { w =>
    w.containers.get(serial) match {
      case Some(c) => w.copy(containers = w.containers - c.serial, item2container = w.item2container -- c.items.keys)
      case None => w
    }
  }

  def deleteContainerItem(serial: Serial): World.State = World.modify { w =>
    w.item2container.get(serial).flatMap(w.containers.get) match {
      case Some(c) => w.copy(containers = w.containers.updated(c.serial,  c.copy(items = c.items - serial)), item2container = w.item2container - serial)
      case None => w
    }
  }

  def itemHash(p: ItemRevisionHashPacket): World.State = World.modify { w =>
    (for {
      cid <- w.item2container.get(p.serial)
      c <- w.containers.get(cid)
      i <- c.items.get(p.serial)
    } yield {
      w.copy(containers = w.containers.updated(c.serial, c.copy(items = c.items.updated(i.serial, i.copy(itemHash = p.itemHash)))))
    }).getOrElse(w)
  }
}

package kdkvsk.hackuo.model.composition

import cats.data.State
import com.typesafe.scalalogging.LazyLogging
import kdkvsk.hackuo._
import kdkvsk.hackuo.model.common.{GraphicId, GumpId, Serial}
import kdkvsk.hackuo.model.{Item, World}
import kdkvsk.hackuo.network.packets.recv._

import scala.language.implicitConversions

case class ContainerItem(serial: Serial, graphicId: GraphicId, amount: Int,
                         x: Int, y: Int, gridIndexOpt: Option[Int],
                         hue: Int, containerSerial: Serial, itemHash: Int) extends Item

case class Container(serial: Serial, gumpId: GumpId, items: Map[Serial, ContainerItem] = Map.empty)

case class ContainerData(containers: Map[Serial, Container], item2container: Map[Serial, Serial]) extends Part[ContainerData]

object ContainerData extends PartOps[World, ContainerData] with LazyLogging {
  def get(w: World): ContainerData = w.containers
  def set(w: World, t: ContainerData): World = w.copy(containers = t)

  val handler: PartialFunction[Message, State[ContainerData, Response]] = {
    case p: x24_DrawContainerPacket => drawContainer(p)
    case p: x3C_AddContainerItemsPacket => addContainerItems(p)
    case p: x25_AddItemToContainerPacket => addItemToContainer(p)
    case p: x1D_DeletePacket => deleteItem(p)
    case p: xDC_ItemRevisionHashPacket => itemHash(p)
  }

  def drawContainer(p: x24_DrawContainerPacket): State[ContainerData, Event] = State { s =>
    val c: Container = Container(p.serial, p.gumpId, Map.empty)
    (s.copy(containers = s.containers.updated(p.serial, c)), ContainerOpenedEvent(c))
  }

  def addContainerItems(p: x3C_AddContainerItemsPacket): State[ContainerData, Seq[Event]] = State { world =>
    p.items.groupBy(_.containerSerial).foldLeft((world, Seq.empty[Event])) {
      case ((w, events), (cid, its)) =>
        w.containers.get(cid) match {
          case Some(c) =>
            val newItems: Map[Serial, ContainerItem] = its.map { i =>
              i.serial -> ContainerItem(i.serial, i.graphicId, i.amount,
                i.x, i.y, i.gridIndexOpt.map(_.intValue()),
                i.hue, i.containerSerial, itemHash = 0)
            }.toMap
            val i2c: Map[Serial, Serial] = newItems.values.map(i => i.serial -> i.containerSerial).toMap
            val newEvents: Seq[Event] = newItems.values.map(i => ContainerItemAddedEvent(i)).toVector ++ events
            (w.copy(containers = w.containers.updated(c.serial, c.copy(items = c.items ++ newItems)), item2container = w.item2container ++ i2c), newEvents)
          case None =>
            logger.warn(s"container $cid does not exist - while adding items to container")
            (w, events)
        }
    }
  }

  def addItemToContainer(p: x25_AddItemToContainerPacket): State[ContainerData, Event] = State { w =>
    val c: Container = w.containers.getOrElse(p.containerSerial, Container(p.containerSerial, GumpId(0), Map.empty))
    val i: ContainerItem = ContainerItem(p.serial, p.graphicId, p.amount,
      p.x, p.y, p.gridIndexOpt.map(_.intValue()), p.hue, p.containerSerial, 0x0)
    val r: ContainerItemAddedEvent = ContainerItemAddedEvent(i)
    (w.copy(containers = w.containers.updated(c.serial, c.copy(items = c.items.updated(i.serial, i))),
      item2container = w.item2container.updated(i.serial, c.serial)), r)
  }

  def deleteItem(p: x1D_DeletePacket): State[ContainerData, Option[Event]] = {
    for {
      o1 <- deleteContainer(p.serial)
      o2 <- deleteContainerItem(p.serial)
    } yield {
      o1.orElse(o2)
    }
  }

  def deleteContainer(serial: Serial): State[ContainerData, Option[Event]] = State { w =>
    w.containers.get(serial) match {
      case Some(c) => (w.copy(containers = w.containers - c.serial, item2container = w.item2container -- c.items.keys), Some(ContainerClosedEvent(c)))
      case None => (w, None)
    }
  }

  def deleteContainerItem(serial: Serial): State[ContainerData, Option[Event]] = State { w =>
    w.item2container.get(serial).flatMap(w.containers.get).flatMap(_.items.get(serial)) match {
      case Some(i) =>
        val c: Container = w.containers(i.containerSerial)
        (w.copy(containers = w.containers.updated(i.containerSerial,  c.copy(items = c.items - i.serial)), item2container = w.item2container - i.serial), Some(ContainerItemRemovedEvent(i)))
      case None => (w, None)
    }
  }

  def itemHash(p: xDC_ItemRevisionHashPacket): State[ContainerData, Unit] = State.modify { w =>
    (for {
      cid <- w.item2container.get(p.serial)
      c <- w.containers.get(cid)
      i <- c.items.get(p.serial)
    } yield {
      w.copy(containers = w.containers.updated(c.serial, c.copy(items = c.items.updated(i.serial, i.copy(itemHash = p.itemHash)))))
    }).getOrElse(w)
  }
}

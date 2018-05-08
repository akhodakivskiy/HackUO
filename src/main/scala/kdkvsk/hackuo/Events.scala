package kdkvsk.hackuo

import kdkvsk.hackuo.model.composition.{Container, ContainerItem}
import kdkvsk.hackuo.model.common.Serial
import kdkvsk.hackuo.network.packets.recvsend.ClilocItem

sealed trait Event

case class ContainerOpenedEvent(container: Container) extends Event
case class ContainerClosedEvent(container: Container) extends Event
case class ContainerItemAddedEvent(item: ContainerItem) extends Event
case class ContainerItemRemovedEvent(item: ContainerItem) extends Event

case class MovementStepEvent(x: Int, y: Int) extends Event

case class ClilocEvent(serial: Serial, items: Seq[ClilocItem]) extends Event

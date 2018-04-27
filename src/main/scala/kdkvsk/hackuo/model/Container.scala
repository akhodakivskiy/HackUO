package kdkvsk.hackuo.model

import kdkvsk.hackuo.model.common.{GraphicId, GumpId, Serial}

case class ContainerItem(serial: Serial, graphicId: GraphicId, amount: Int,
                         x: Int, y: Int, gridIndexOpt: Option[Int],
                         hue: Int, containerSerial: Serial, itemHash: Int) extends Item

case class Container(serial: Serial, gumpId: GumpId, items: Map[Serial, ContainerItem] = Map.empty)

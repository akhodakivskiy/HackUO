package kdkvsk.hackuo.model

import kdkvsk.hackuo.model.common.{Direction, GraphicId, ItemLayer, Serial}

trait Item {
  def serial: Serial
  def graphicId: GraphicId
  def itemHash: Int
}

case class ObjectItem(serial: Serial, graphicId: GraphicId, amount: Int,
                      x: Int, y: Int, z: Int,
                      light: Byte, hue: Int, direction: Direction.Type, flag: Int,
                      itemHash: Int) extends Item

case class MultiItem(serial: Serial, graphicId: GraphicId, x: Int, y: Int, z: Int, itemHash: Int) extends Item

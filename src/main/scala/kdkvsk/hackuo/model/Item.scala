package kdkvsk.hackuo.model

import kdkvsk.hackuo.model.common.{Direction, ItemLayer, Serial}

trait Item[T <: Item[T]] {
  def itemHash: Int
  def withItemHash(value: Int): T
}

case class ObjectItem(serial: Serial, itemId: Int, amount: Int,
                      x: Int, y: Int, z: Int,
                      layer: ItemLayer.Type, hue: Int, direction: Direction.Type, flag: Int,
                      itemHash: Int) extends Item[ObjectItem] {
  def withItemHash(value: Int): ObjectItem = copy(itemHash = value)
}

case class MultiItem(serial: Serial, itemId: Int, x: Int, y: Int, z: Int, itemHash: Int) extends Item[MultiItem] {
  def withItemHash(value: Int): MultiItem = copy(itemHash = value)
}

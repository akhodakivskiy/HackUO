package kdkvsk.hackuo.model

import kdkvsk.hackuo.model.common._


object Notoriety extends Enumeration {
  type Type = Value

  val Innocent = Value(1, "innocent")
  val Ally = Value(2, "ally")
  val CanBeAttacked = Value(3, "canBeAttacked")
  val Criminal = Value(4, "criminal")
  val Enemy = Value(5, "enemy")
  val Murderer = Value(6, "murderer")
  val Invulnerable = Value(7, "invulnerable")
}

case class MobileItem(serial: Serial, graphicId: GraphicId, layer: ItemLayer.Type, hue: Int, itemHash: Int) extends Item

case class Mobile(serial: Serial,
                  bodyId: BodyId,
                  hue: Int = 0,
                  x: Int = 0,
                  y: Int = 0,
                  z: Int = 0,
                  flags: Int = 0,
                  direction: Direction.Type = null,
                  notoriety: Notoriety.Type = null,
                  warMode: Boolean = false,
                  items: Map[Serial, MobileItem] = Map.empty,
                  mapId: Int = 0,
                  name: String = "",
                  hits: Int = 0, maxHits: Int = 0,
                  stamina: Int = 0, maxStamina: Int = 0,
                  mana: Int = 0, maxMana: Int = 0,
                  strength: Int = 0, dexterity: Int = 0, intelligence: Int = 0,
                  gold: Int = 0,
                  weight: Int = 0, maxWeight: Int = 0,
                  followers: Int = 0, maxFollowers: Int = 0,
                  physicalResist: Int = 0, fireResist: Int = 0, coldResist: Int = 0, poisonResist: Int = 0, energyResist: Int = 0,
                  luck: Int = 0, damageMin: Int = 0, damageMax: Int = 0) extends Point3D {
  val backpackOpt: Option[MobileItem] = items.values.find(_.layer == ItemLayer.Backpack)
}

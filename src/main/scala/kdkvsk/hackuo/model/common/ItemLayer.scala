package kdkvsk.hackuo.model.common

object ItemLayer extends Enumeration {
  type Type = Value

  val Invalid = Value(0x00, "Invalid")
  val OneHanded = Value(0x01, "OneHanded")
  val TwoHanded = Value(0x02, "TwoHanded")
  val Shoes = Value(0x03, "Shoes")
  val Pants = Value(0x04, "Pants")
  val Shirt = Value(0x05, "Shirt")
  val Helm = Value(0x06, "Helm")
  val Gloves = Value(0x07, "Gloves")
  val Ring = Value(0x08, "Ring")
  val Talisman = Value(0x09, "Talisman")
  val Neck = Value(0x0A, "Neck")
  val Hair = Value(0x0B, "Hair")
  val Waist = Value(0x0C, "Waist")
  val InnerTorso = Value(0x0D, "InnerTorso")
  val Bracelet = Value(0x0E, "Bracelet")
  val Face = Value(0x0F, "Face")
  val FacialHair = Value(0x10, "FacialHair")
  val MiddleTorso = Value(0x11, "MiddleTorso")
  val Earrings = Value(0x12, "Earrings")
  val Arms = Value(0x13, "Arms")
  val Cloak = Value(0x14, "Cloak")
  val Backpack = Value(0x15, "Backpack")
  val OuterTorso = Value(0x16, "OuterTorso")
  val OuterLegs = Value(0x17, "OuterLegs")
  val InnerLegs = Value(0x18, "InnerLegs")
  val Mount = Value(0x19, "Mount")
  val ShopBuy = Value(0x1A, "ShopBuy")
  val ShopResale = Value(0x1B, "ShopResale")
  val ShopSell = Value(0x1C, "ShopSell")
  val Bank = Value(0x1D, "Bank")
  val Reserved_1 = Value(0x1E, "Reserved_1")
  val SecureTrade = Value(0x1F,  "SecureTrade")

  val FirstValid = OneHanded
  val LastUserValid = InnerLegs
}

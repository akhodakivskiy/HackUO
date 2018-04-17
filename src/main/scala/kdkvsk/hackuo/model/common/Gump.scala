package kdkvsk.hackuo.model.common

object GumpButtonType extends Enumeration {
  type Type = Value

  val Page = Value(0, "page")
  val Reply = Value(1, "reply")

  def unapply(index: Int): Option[Type] = Some(apply(index))
}

object GumpHtmlLocalizedType extends Enumeration {
  type Type = Value

  val Plain = Value("xmfhtmlgump")
  val Color = Value("xmfhtmlgumpcolor")
  val Args = Value("xmfhtmltok")

  def unapply(index: Int): Option[Type] = Some(apply(index))
}

trait GumpEntry

case class GumpAlphaRegion(x: Int, y: Int, width: Int, height: Int) extends GumpEntry
case class GumpBackground(x: Int, y: Int, width: Int, height: Int, gumpId: Int) extends GumpEntry
case class GumpButton(x: Int, y: Int, id1: Int, id2: Int, buttonType: GumpButtonType.Type, param: Int, buttonId: Int) extends GumpEntry
case class GumpCheck(x: Int, y: Int, id1: Int, id2: Int, initialState: Boolean, switchId: Int) extends GumpEntry
case class GumpGroup(group: Int) extends GumpEntry
case class GumpHtml(x: Int, y: Int, width: Int, height: Int, text: String, background: Boolean, scrollbar: Boolean) extends GumpEntry
case class GumpHtmlLocalized(x: Int, y: Int, width: Int, height: Int, number: Int, background: Boolean, scrollbar: Boolean, color: Option[Int], argsOpt: Option[String], gumpType: GumpHtmlLocalizedType.Type) extends GumpEntry
case class GumpImage(x: Int, y: Int, gumpId: Int, hueOpt: Option[Int]) extends GumpEntry
case class GumpImageTileButton(x: Int, y: Int, id1: Int, id2: Int, buttonId: Int, buttonType: GumpButtonType.Type, param: Int, itemId: Int, hue: Int, width: Int, height: Int, localizedTooltipOpt: Option[Int]) extends GumpEntry
case class GumpImageTiled(x: Int, y: Int, width: Int, height: Int, gumpId: Int) extends GumpEntry
case class GumpItem(x: Int, y: Int, itemId: Int, hueOpt: Option[Int]) extends GumpEntry
case class GumpItemProperty(serial: Int) extends GumpEntry
case class GumpLabel(x: Int, y: Int, hue: Int, text: String) extends GumpEntry
case class GumpLabelCropped(x: Int, y: Int, width: Int, height: Int, hue: Int, text: String) extends GumpEntry
case class GumpPage(page: Int) extends GumpEntry
case class GumpRadio(x: Int, y: Int, id1: Int, id2: Int, initialState: Boolean, switchId: Int) extends GumpEntry
case class GumpTextEntry(x: Int, y: Int, width: Int, height: Int, hue: Int, entryId: Int, initialText: String) extends GumpEntry
case class GumpTextEntryLimited(x: Int, y: Int, width: Int, height: Int, hue: Int, entryId: Int, initialText: String, size: Int) extends GumpEntry
case class GumpTooltip(number: Int, argsOpt: Option[String]) extends GumpEntry

case class Gump(serial: Int, gumpId: Int, x: Int, y: Int, draggable: Boolean, closeable: Boolean, resizeable: Boolean, disposable: Boolean, entries: Seq[GumpEntry])

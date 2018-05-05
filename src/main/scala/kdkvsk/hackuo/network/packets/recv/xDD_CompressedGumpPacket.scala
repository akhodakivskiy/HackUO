package kdkvsk.hackuo.network.packets.recv

import java.io.{ByteArrayInputStream, DataInputStream}
import java.nio.charset.StandardCharsets
import java.util.zip.InflaterInputStream

import kdkvsk.hackuo.client.IntValue
import kdkvsk.hackuo.model._
import kdkvsk.hackuo.model.common._
import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class xDD_CompressedGumpPacket(serial: Int, gumpId: Int, x: Int, y: Int, data: String, texts: Seq[String]) extends RecvPacket {
  val id: Int = xDD_CompressedGumpPacketParser.packetId

  def gump: Gump = xDD_CompressedGumpPacketParser.assembleGump(this)
}

object xDD_CompressedGumpPacketParser extends RecvPacketParser {
  val packetId: Int = 0xDD

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    ensureLength(data, size)

    val serial: Int = data.readInt()
    val gumpId: Int = data.readInt()
    val x: Int = data.readInt()
    val y: Int = data.readInt()
    val clen: Int = data.readInt()
    val dlen: Int = data.readInt()
    val cdata: Array[Byte] = new Array[Byte](clen - 4)
    val ddata: Array[Byte] = new Array[Byte](dlen)
    if (data.read(cdata) != clen - 4) {
      throw new IllegalStateException(s"failed to read $clen compressed bytes")
    }
    val iis = new InflaterInputStream(new ByteArrayInputStream(cdata))
    if (iis.read(ddata) != dlen) {
      throw new IllegalStateException(s"failed to inflate $dlen bytes from $clen compressed bytes")
    }

    val gumpData: String = new String(ddata, 0, dlen - 1, StandardCharsets.US_ASCII)

    val numTextLines: Int = data.readInt()
    val lines: Array[String] = new Array[String](numTextLines)

    if (numTextLines > 0) {
      val clenTxt: Int = data.readInt()
      val dlenTxt: Int = data.readInt()
      val cdataTxt: Array[Byte] = new Array[Byte](clenTxt - 4)
      if (data.read(cdataTxt) != clenTxt - 4) {
        throw new IllegalStateException(s"failed to read $clenTxt bytes of compressed text entries")
      }
      val ddataTxt: Array[Byte] = new Array[Byte](dlenTxt)
      val iisTxt = new InflaterInputStream(new ByteArrayInputStream(cdataTxt))
      if (iisTxt.read(ddataTxt) != dlenTxt) {
        throw new IllegalStateException(s"failed to inflate $dlenTxt bytes of compressed text entries from $clenTxt compressed bytes")
      }

      val baisTxt = new DataInputStream(new ByteArrayInputStream(ddataTxt))

      Range(0, numTextLines).foreach { index =>
        val len: Short = baisTxt.readShort()
        val data: Seq[Char] = Range(0, len).map(_ => baisTxt.readShort().toChar)
        val line: String = data.mkString("")
        lines(index) = line
      }
    }

    val packet = xDD_CompressedGumpPacket(serial, gumpId, x, y, gumpData, lines.toVector)

    packet
  }

  object B {
    def unapply(str: String): Option[Boolean] = IntValue.unapply(str).filter(a => a == 0 || a == 1).map(_ == 1)
  }

  object IV {
    def unapply(str: String): Option[Int] = IntValue.unapply(str)
  }

  object STR {
    def unapply(str: String): Option[String] = Some(str).filter(s => s.startsWith("@") && s.endsWith("@")).map(_.stripPrefix("@").stripSuffix("@"))
  }

  object HUE {
    def unapply(str: String): Option[Int] = Some(str).filter(s => s.startsWith("hue=")).map(_.stripPrefix("hue=")).map(_.toInt)
  }

  def assembleGump(packet: xDD_CompressedGumpPacket): Gump = {
    val init: Gump = Gump(packet.serial, packet.gumpId, packet.x, packet.y, draggable = true, closeable = true, resizeable = true, disposable = true, Vector.empty)

    val partialEntries: Iterable[List[String]] = packet.data
      .stripPrefix("{ ").stripSuffix(" }").split(" \\}\\{ ")
      .map(_.split(" ").toList)

    partialEntries.foldLeft(init) {
      case (g, "nomove" :: Nil) => g.copy(draggable = false)
      case (g, "noclose" :: Nil) => g.copy(closeable = false)
      case (g, "noresiza" :: Nil) => g.copy(resizeable = false)
      case (g, "nodispose" :: Nil) => g.copy(disposable = false)
      case (g, "checkertrans" :: IV(x) :: IV(y) :: IV(width) ::IV(height) :: Nil) =>
        g.copy(entries = g.entries :+ GumpAlphaRegion(x, y, width, height))
      case (g, "resizepic" :: IV(x) :: IV(y) :: IV(gumpId) :: IV(width) ::IV(height) :: Nil) =>
        g.copy(entries = g.entries :+ GumpBackground(x, y, gumpId, width, height))
      case (g, "button" :: IV(x) :: IV(y) :: IV(id1) :: IV(id2) :: IV(GumpButtonType(buttonType)) :: IV(param) :: IV(buttonId) :: Nil) =>
        g.copy(entries = g.entries :+ GumpButton(x, y, id1, id2, buttonType, param, buttonId))
      case (g, "checkbox" :: IV(x) :: IV(y) :: IV(id1) :: IV(id2) :: B(initialState) :: IV(switchId) :: Nil) =>
        g.copy(entries = g.entries :+ GumpCheck(x, y, id1, id2, initialState, switchId))
      case (g, "group" :: IV(groupId) :: Nil) =>
        g.copy(entries = g.entries :+ GumpGroup(groupId))
      case (g, "htmlgump" :: IV(x) :: IV(y) :: IV(width) :: IV(height) :: IV(textId) :: B(background) :: B(scrollbar) :: Nil) =>
        g.copy(entries = g.entries :+ GumpHtml(x, y, width, height, packet.texts(textId), background, scrollbar))
      case (g, "xmfhtmlgump" :: IV(x) :: IV(y) :: IV(width) :: IV(height) :: IV(number) :: B(background) :: B(scrollbar) :: Nil) =>
        g.copy(entries = g.entries :+ GumpHtmlLocalized(x, y, width, height, number, background, scrollbar, None, None, GumpHtmlLocalizedType.Plain))
      case (g, "xmfhtmlgumpcolor" :: IV(x) :: IV(y) :: IV(width) :: IV(height) :: IV(number) :: B(background) :: B(scrollbar) :: IV(color) :: Nil) =>
        g.copy(entries = g.entries :+ GumpHtmlLocalized(x, y, width, height, number, background, scrollbar, None, None, GumpHtmlLocalizedType.Color))
      case (g, "xmfhtmltok" :: IV(x) :: IV(y) :: IV(width) :: IV(height) :: B(background) :: B(scrollbar) :: IV(color) :: IV(number) :: STR(args) :: Nil) =>
        g.copy(entries = g.entries :+ GumpHtmlLocalized(x, y, width, height, number, background, scrollbar, Some(color), Some(args), GumpHtmlLocalizedType.Args))
      case (g, "gumppic" :: IV(x) :: IV(y) :: IV(gumpId) :: Nil) =>
        g.copy(entries = g.entries :+ GumpImage(x, y, gumpId, None))
      case (g, "gumppic" :: IV(x) :: IV(y) :: IV(gumpId) :: HUE(hue) :: Nil) =>
        g.copy(entries = g.entries :+ GumpImage(x, y, gumpId, Some(hue)))
      case (g, "buttontileart" :: IV(x) :: IV(y) :: IV(id1) :: IV(id2) :: IV(GumpButtonType(buttonType)) :: IV(param) :: IV(buttonId) :: IV(itemId) :: IV(hue) :: IV(width) :: IV(height) :: Nil) =>
        g.copy(entries = g.entries :+ GumpImageTileButton(x, y, id1, id2, buttonId, buttonType, param, itemId, hue, width, height, None))
      case (g, "gumppictiled" :: IV(x) :: IV(y) :: IV(width) :: IV(height) :: IV(gumpId) :: Nil) =>
        g.copy(entries = g.entries :+ GumpImageTiled(x, y, width, height, gumpId))
      case (g, "tilepic" :: IV(x) :: IV(y) :: IV(itemId) :: Nil) =>
        g.copy(entries = g.entries :+ GumpItem(x, y, itemId, None))
      case (g, "tilepichue" :: IV(x) :: IV(y) :: IV(itemId) :: IV(hue) :: Nil) =>
        g.copy(entries = g.entries :+ GumpItem(x, y, itemId, Some(hue)))
      case (g, "itemproperty" :: IV(serial) :: Nil) =>
        g.copy(entries = g.entries :+ GumpItemProperty(serial))
      case (g, "text" :: IV(x) :: IV(y) :: IV(hue) :: IV(textId) :: Nil) if packet.texts.size < textId =>
        g.copy(entries = g.entries :+ GumpLabel(x, y, hue, packet.texts(textId)))
      case (g, "text" :: IV(x) :: IV(y) :: IV(hue) :: IV(textId) :: Nil) =>
        g.copy(entries = g.entries :+ GumpLabel(x, y, hue, s"[$textId]"))
      case (g, "croppedtext" :: IV(x) :: IV(y) :: IV(width) :: IV(height) :: IV(hue) :: IV(textId) :: Nil) if packet.texts.length < textId =>
        g.copy(entries = g.entries :+ GumpLabelCropped(x, y, width, height, hue, packet.texts(textId)))
      case (g, "croppedtext" :: IV(x) :: IV(y) :: IV(width) :: IV(height) :: IV(hue) :: IV(textId) :: Nil) =>
        g.copy(entries = g.entries :+ GumpLabelCropped(x, y, width, height, hue, s"[textId]"))
      case (g, "page" :: IV(page) :: Nil) =>
        g.copy(entries = g.entries :+ GumpPage(page))
      case (g, "radio" :: IV(x) :: IV(y) :: IV(id1) :: IV(id2) :: B(initialState) :: IV(switchId) :: Nil) =>
        g.copy(entries = g.entries :+ GumpRadio(x, y, id1, id2, initialState, switchId))
      case (g, "textentry" :: IV(x) :: IV(y) :: IV(width) :: IV(height) :: IV(hue) :: IV(entryId) :: IV(textId) :: Nil) if packet.texts.length < textId =>
        g.copy(entries = g.entries :+ GumpTextEntry(x, y, width, height, hue, entryId, packet.texts(textId)))
      case (g, "textentry" :: IV(x) :: IV(y) :: IV(width) :: IV(height) :: IV(hue) :: IV(entryId) :: IV(textId) :: Nil) =>
        g.copy(entries = g.entries :+ GumpTextEntry(x, y, width, height, hue, entryId, s"[$textId]"))
      case (g, "textentrylimited" :: IV(x) :: IV(y) :: IV(width) :: IV(height) :: IV(hue) :: IV(entryId) :: IV(textId) :: IV(size) :: Nil) if packet.texts.length < textId =>
        g.copy(entries = g.entries :+ GumpTextEntryLimited(x, y, width, height, hue, entryId, packet.texts(textId), size))
      case (g, "textentrylimited" :: IV(x) :: IV(y) :: IV(width) :: IV(height) :: IV(hue) :: IV(entryId) :: IV(textId) :: IV(size) :: Nil) =>
        g.copy(entries = g.entries :+ GumpTextEntryLimited(x, y, width, height, hue, entryId, s"[$textId]", size))
      case (g, "tooltip" :: IV(number) :: Nil) if g.entries.lastOption.exists(_.isInstanceOf[GumpImageTileButton]) =>
        g.copy(entries = g.entries.init :+ g.entries.last.asInstanceOf[GumpImageTileButton].copy(localizedTooltipOpt = Some(number)))
      case (g, "tooltip" :: IV(number) :: Nil) =>
        g.copy(entries = g.entries :+ GumpTooltip(number, None))
      case (g, "tooltip" :: IV(number) :: STR(args) :: Nil) =>
        g.copy(entries = g.entries :+ GumpTooltip(number, Some(args)))
      case (g, name :: tail) =>
        throw new NotImplementedError(s"unhandled gump entry '$name', tail: ${tail.mkString("{", " }{ ", "}")}")
    }
  }
}

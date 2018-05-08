package kdkvsk.hackuo.model.composition
import cats.data.State
import kdkvsk.hackuo.model.common.Serial
import kdkvsk.hackuo.network.packets.recvsend._
import kdkvsk.hackuo.network.packets.send.{x06_DoubleClickPacket, x09_SingleClickPacket}
import kdkvsk.hackuo._
import kdkvsk.hackuo.model.{Popup, PopupEntry, World}

case class InteractionData(popupOpt: Option[Popup], clilocs: Map[Serial, Seq[ClilocItem]]) extends Part[InteractionData]

object InteractionData extends PartOps[World, InteractionData] {
  def get(w: World): InteractionData = w.interaction
  def set(w: World, t: InteractionData): World = w.copy(interaction = t)

  val handler: PartialFunction[Message, State[InteractionData, Response]] = {
    case SingleClickInput(serial) => singleClick(serial)
    case DoubleClickInput(serial) => doubleClick(serial)
    case RequestPopupInput(serial) => requestPopup(serial)
    case SelectPopupEntryInput(index) => selectPopupEntry(index)
    case PacketMessage(p: xBF_14_ShowPopupPacket) => showPopup(p)
    case RequestClilocInput(serials) => requestCliloc(serials)
    case PacketMessage(p: xD6_ClilocResponsePacket) => showCliloc(p)
  }

  def singleClick(serial: Serial): State[InteractionData, Response] = State.pure(PacketResponse(x09_SingleClickPacket(serial.value)))

  def doubleClick(serial: Serial): State[InteractionData, Response] = State.pure(PacketResponse(x06_DoubleClickPacket(serial.value)))

  def requestPopup(serial: Serial): State[InteractionData, Response] = State.pure(PacketResponse(xBF_13_RequestPopupPacket(serial.value)))

  def selectPopupEntry(index: Int): State[InteractionData, Response] = State.inspect { w =>
    w.popupOpt match {
      case Some(p) => PacketResponse(xBF_15_SelectPopupEntryPacket(p.serial.value, index.shortValue()))
      case None => NoopResponse
    }
  }

  def showPopup(p: xBF_14_ShowPopupPacket): State[InteractionData, Unit] = State.modify { w =>
    val entries: Seq[PopupEntry] = p.entries.map { e =>
      PopupEntry(e.index, e.textId, e.flags, e.hueOpt.map(_.intValue()))
    }
    val popup = Popup(Serial(p.serial), entries)
    w.copy(popupOpt = Some(popup))
  }

  def requestCliloc(serials: Seq[Serial]): State[InteractionData, Response] = State.pure(PacketResponse(xD6_RequestClilocPacket(serials)))

  def showCliloc(p: xD6_ClilocResponsePacket): State[InteractionData, Response] = State { w =>
    (w.copy(clilocs = w.clilocs.updated(p.serial, p.items)), EventResponse(ClilocEvent(p.serial, p.items)))
  }
}

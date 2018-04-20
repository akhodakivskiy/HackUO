package kdkvsk.hackuo.handler

import cats.data.State
import com.typesafe.scalalogging.LazyLogging
import kdkvsk.hackuo.model.common.Serial
import kdkvsk.hackuo._
import kdkvsk.hackuo.model.{Popup, PopupEntry, World}
import kdkvsk.hackuo.network.packets.recv.ClilocResponsePacket
import kdkvsk.hackuo.network.packets.recvsend.{GiRequestPopupPacket, GiSelectPopupEntryPacket, GiShowPopupPacket}
import kdkvsk.hackuo.network.packets.send.{DoubleClickPacket, RequestClilocPacket, SingleClickPacket}

case class SingleClickMessage(serial: Serial) extends HandlerMessage
case class DoubleClickMessage(serial: Serial) extends HandlerMessage
case class RequestPopupMessage(serial: Serial) extends HandlerMessage
case class SelectPopupEntryMessage(index: Int) extends HandlerMessage
case class RequestClilocMessage(serials: Seq[Serial]) extends HandlerMessage

object InteractionHandler extends Handler with PopupHandlerOps {
  val handle: PartialFunction[Message, State[World, Response]] = {
    case SingleClickMessage(serial) => singleClick(serial)
    case DoubleClickMessage(serial) => doubleClick(serial)
    case RequestPopupMessage(serial) => requestPopup(serial)
    case SelectPopupEntryMessage(index) => selectPopupEntry(index)
    case PacketMessage(p: GiShowPopupPacket) => showPopup(p)
    case RequestClilocMessage(serials) => requestCliloc(serials)
    case PacketMessage(p: ClilocResponsePacket) => showCliloc(p)
  }
}

trait PopupHandlerOps extends LazyLogging {
  def singleClick(serial: Serial): World.State = State.pure(PacketResponse(SingleClickPacket(serial.value)))

  def doubleClick(serial: Serial): World.State = State.pure(PacketResponse(DoubleClickPacket(serial.value)))

  def requestPopup(serial: Serial): World.State = State.pure(PacketResponse(GiRequestPopupPacket(serial.value)))

  def selectPopupEntry(index: Int): World.State = State.inspect { w =>
    w.popupOpt match {
      case Some(p) => PacketResponse(GiSelectPopupEntryPacket(p.serial.value, index.shortValue()))
      case None => NoopResponse
    }
  }

  def showPopup(p: GiShowPopupPacket): World.State = World.modify { w =>
    val entries: Seq[PopupEntry] = p.entries.map { e =>
      PopupEntry(e.index, e.textId, e.flags, e.hueOpt.map(_.intValue()))
    }
    val popup = Popup(Serial(p.serial), entries)
    w.copy(popupOpt = Some(popup))
  }

  def requestCliloc(serials: Seq[Serial]): World.State = State.pure(PacketResponse(RequestClilocPacket(serials)))

  def showCliloc(p: ClilocResponsePacket): World.State = State.inspect { w =>
    MultiResponse(
      p.items.toList.map { i =>
        LogResponse(s"${i.clilocId} - ${i.textOpt.getOrElse("-")}")
      }
    )
  }
}
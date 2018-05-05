package kdkvsk.hackuo.handler

import cats.data.State
import com.typesafe.scalalogging.LazyLogging
import kdkvsk.hackuo._
import kdkvsk.hackuo.model.common.Serial
import kdkvsk.hackuo.model.{Popup, PopupEntry, World}
import kdkvsk.hackuo.network.packets.recvsend._
import kdkvsk.hackuo.network.packets.send.{x06_DoubleClickPacket, x09_SingleClickPacket}

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
    case PacketMessage(p: xBF_14_ShowPopupPacket) => showPopup(p)
    case RequestClilocMessage(serials) => requestCliloc(serials)
    case PacketMessage(p: xD6_ClilocResponsePacket) => showCliloc(p)
  }
}

trait PopupHandlerOps extends LazyLogging {
  def singleClick(serial: Serial): World.State = State.pure(PacketResponse(x09_SingleClickPacket(serial.value)))

  def doubleClick(serial: Serial): World.State = State.pure(PacketResponse(x06_DoubleClickPacket(serial.value)))

  def requestPopup(serial: Serial): World.State = State.pure(PacketResponse(xBF_13_RequestPopupPacket(serial.value)))

  def selectPopupEntry(index: Int): World.State = State.inspect { w =>
    w.popupOpt match {
      case Some(p) => PacketResponse(xBF_15_SelectPopupEntryPacket(p.serial.value, index.shortValue()))
      case None => NoopResponse
    }
  }

  def showPopup(p: xBF_14_ShowPopupPacket): World.State = World.modify { w =>
    val entries: Seq[PopupEntry] = p.entries.map { e =>
      PopupEntry(e.index, e.textId, e.flags, e.hueOpt.map(_.intValue()))
    }
    val popup = Popup(Serial(p.serial), entries)
    w.copy(popupOpt = Some(popup))
  }

  def requestCliloc(serials: Seq[Serial]): World.State = State.pure(PacketResponse(xD6_RequestClilocPacket(serials)))

  def showCliloc(p: xD6_ClilocResponsePacket): World.State = State.inspect { w =>
    MultiResponse(
      p.items.toList.map { i =>
        LogResponse(s"${i.clilocId} - ${i.textOpt.getOrElse("-")}")
      }
    )
  }
}
package kdkvsk.hackuo.handler

import cats.data.State
import kdkvsk.hackuo.client.IntValue
import kdkvsk.hackuo.model.World
import kdkvsk.hackuo._
import kdkvsk.hackuo.model.common.Serial
import kdkvsk.hackuo.network.packets.recvsend.{xBF_13_RequestPopupPacket, xBF_15_SelectPopupEntryPacket}
import kdkvsk.hackuo.network.packets.send.x06_DoubleClickPacket

case class REPLMessage(command: List[String]) extends HandlerMessage

object REPLHandler extends Handler with REPLHandlerOps {
  val handle: PartialFunction[Message, State[World, Response]] = {
    case REPLMessage(command) => State { w =>
      (w, handleCommand(w, command))
    }
  }
}

trait REPLHandlerOps {
  def handleCommand(w: World, command: List[String]): Response = {
    command match {
      case Nil => NoopResponse
      case "exit" :: Nil => TerminateResponse("terminated by user")
      case "quit" :: Nil => TerminateResponse("terminated by user")
      case "walk" :: IntValue(dx) :: IntValue(dy) :: Nil => MessageResponse(StartMoveHandlerMessage(dx, dy, isRunning = false, 0))
      case "run" :: IntValue(dx) :: IntValue(dy) :: Nil => MessageResponse(StartMoveHandlerMessage(dx, dy, isRunning = true, 0))
      case "syncpos" :: Nil => MessageResponse(SyncMoveHandlerMessage)
      case "paperdoll" :: Nil => PacketResponse(x06_DoubleClickPacket(w.playerSerial.value))
      case "backpack" :: Nil => PacketResponse(x06_DoubleClickPacket(w.player.backpackOpt.getOrElse(throw new IllegalStateException(s"player doesn't have a backpack")).serial.value))
      case "click" :: IntValue(serial) :: Nil => MessageResponse(SingleClickMessage(Serial(serial)))
      case "doubleclick" :: IntValue(serial) :: Nil => MessageResponse(DoubleClickMessage(Serial(serial)))
      case "popup" :: IntValue(serial) :: Nil => MessageResponse(RequestPopupMessage(Serial(serial)))
      case "popup" :: "self" :: Nil => MessageResponse(RequestPopupMessage(w.playerSerial))
      case "popup" :: "respond" :: IntValue(index) :: Nil => MessageResponse(SelectPopupEntryMessage(index))
      case "cliloc" :: IntValue(serial) :: Nil => MessageResponse(RequestClilocMessage(Serial(serial) :: Nil))
      case _ => LogResponse("unrecognized command")
    }
  }
}

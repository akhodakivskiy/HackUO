package kdkvsk.hackuo.handler

import java.net.InetAddress

import cats.data.State
import kdkvsk.hackuo._
import kdkvsk.hackuo.model.World
import kdkvsk.hackuo.model.common.ClientVersion
import kdkvsk.hackuo.network.packets.recv._
import kdkvsk.hackuo.network.packets.recvsend.xBD_ClientVersionPacket
import kdkvsk.hackuo.network.packets.send._

object LoginHandler extends Handler with LoginHandlerOps {
  val handle: PartialFunction[Message, World.State] = {
    case StartupMessage => serverLogin
    case PacketMessage(p: xB9_SupportedFeaturesPacket) => features(p)
    case PacketMessage(p: xA9_CharacterListPacket) => selectChar(p)
    case PacketMessage(p: xBD_ClientVersionPacket) => clientVersion(p)
    case PacketMessage(p: x1B_LoginConfirmPacket) => loginConfirm(p)
  }
}

trait LoginHandlerOps {

  val serverLogin: World.State = State.inspect { w =>
    PacketResponse(x91_GameLoginPacket(w.login.authId, w.login.username, w.login.password))
  }

  def features(packet: xB9_SupportedFeaturesPacket): World.State = State { s =>
    (s.copy(login = s.login.copy(featuresFlag = packet.featuresFlag)), NoopResponse)
  }

  def selectChar(packet: xA9_CharacterListPacket): World.State = State.inspect { w =>
    packet.characters.find(_.name.compareToIgnoreCase(w.login.characterName) == 0) match {
      case Some(char) => PacketResponse(x5D_CharacterLoginPacket(char.name, char.index, w.login.clientFlag, w.login.loginCount, w.login.clientIp))
      case None => TerminateResponse(s"character with name '${w.login.characterName}' not found")
    }
  }

  def clientVersion(packet: xBD_ClientVersionPacket): World.State = State.inspect { s =>
    PacketResponse(xBD_ClientVersionPacket(s.login.clientVersion.stringify))
  }

  def loginConfirm(packet: x1B_LoginConfirmPacket): World.State = State { s =>
    (s.copy(login = s.login.copy(loggedIn = true)), NoopResponse)
  }
}

package kdkvsk.hackuo.handler

import java.net.InetAddress

import cats.data.State
import kdkvsk.hackuo._
import kdkvsk.hackuo.model.World
import kdkvsk.hackuo.model.common.ClientVersion
import kdkvsk.hackuo.network.packets.recv._
import kdkvsk.hackuo.network.packets.recvsend.ClientVersionPacket
import kdkvsk.hackuo.network.packets.send._

object LoginHandler extends Handler with LoginHandlerOps {
  val handle: PartialFunction[Message, World.State] = {
    case ConnectedMessage => serverLogin
    case PacketMessage(p: SupportedFeaturesPacket) => features(p)
    case PacketMessage(p: CharacterListPacket) => selectChar(p)
    case PacketMessage(p: ClientVersionPacket) => clientVersion(p)
    case PacketMessage(p: LoginConfirmPacket) => loginConfirm(p)
  }
}

trait LoginHandlerOps {

  val serverLogin: World.State = State.inspect { w =>
    PacketResponse(GameLoginPacket(w.login.authId, w.login.username, w.login.password))
  }

  def features(packet: SupportedFeaturesPacket): World.State = State { s =>
    (s.copy(login = s.login.copy(featuresFlag = packet.featuresFlag)), NoopResponse)
  }

  def selectChar(packet: CharacterListPacket): World.State = State.inspect { w =>
    packet.characters.find(_.name.compareToIgnoreCase(w.login.characterName) == 0) match {
      case Some(char) => PacketResponse(CharacterLoginPacket(char.name, char.index, w.login.clientFlag, w.login.loginCount, w.login.clientIp))
      case None => TerminateResponse(s"character with name '${w.login.characterName}' not found")
    }
  }

  def clientVersion(packet: ClientVersionPacket): World.State = State.inspect { s =>
    PacketResponse(ClientVersionPacket(s.login.clientVersion.stringify))
  }

  def loginConfirm(packet: LoginConfirmPacket): World.State = State { s =>
    (s.copy(login = s.login.copy(loggedIn = true)), NoopResponse)
  }
}

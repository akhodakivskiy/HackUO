package kdkvsk.hackuo.model.composition

import java.net.InetAddress

import cats.data.State
import kdkvsk.hackuo._
import kdkvsk.hackuo.model.World
import kdkvsk.hackuo.model.common.{ClientFlag, ClientVersion}
import kdkvsk.hackuo.network.packets.recv.{x1B_LoginConfirmPacket, xA9_CharacterListPacket, xB9_SupportedFeaturesPacket}
import kdkvsk.hackuo.network.packets.recvsend.xBD_ClientVersionPacket
import kdkvsk.hackuo.network.packets.send.{x5D_CharacterLoginPacket, x91_GameLoginPacket}

case class LoginData(username: String,
                     password: String,
                     clientIp: InetAddress,
                     serverIp: InetAddress,
                     serverPort: Int,
                     authId: Int,
                     clientVersion: ClientVersion,
                     serverName: String,
                     characterName: String,
                     clientFlag: ClientFlag.ValueSet,
                     loginCount: Int,
                     featuresFlag: Int = 0,
                     loggedIn: Boolean = false) extends Part[LoginData]

object LoginData extends PartOps[World, LoginData] {
  def get(w: World): LoginData = w.login
  def set(w: World, t: LoginData): World = w.copy(login = t)

  val handler: PartialFunction[Message, State[LoginData, Response]] = {
    case StartupMessage => serverLogin
    case PacketMessage(p: xB9_SupportedFeaturesPacket) => features(p)
    case PacketMessage(p: xA9_CharacterListPacket) => selectChar(p)
    case PacketMessage(p: xBD_ClientVersionPacket) => clientVersion(p)
    case PacketMessage(p: x1B_LoginConfirmPacket) => loginConfirm(p)
  }

  def serverLogin: State[LoginData, Response] = State.inspect { w =>
    PacketResponse(x91_GameLoginPacket(w.authId, w.username, w.password))
  }

  def features(packet: xB9_SupportedFeaturesPacket): State[LoginData, Unit] = State.modify { s =>
    s.copy(featuresFlag = packet.featuresFlag)
  }

  def selectChar(packet: xA9_CharacterListPacket): State[LoginData, Response] = State.inspect { w =>
    packet.characters.find(_.name.compareToIgnoreCase(w.characterName) == 0) match {
      case Some(char) => PacketResponse(x5D_CharacterLoginPacket(char.name, char.index, w.clientFlag.toBitMask.head.toInt, w.loginCount, w.clientIp))
      case None => TerminateResponse(s"character with name '${w.characterName}' not found")
    }
  }

  def clientVersion(packet: xBD_ClientVersionPacket): State[LoginData, Response] = State.inspect { s =>
    PacketResponse(xBD_ClientVersionPacket(s.clientVersion.stringify))
  }

  def loginConfirm(packet: x1B_LoginConfirmPacket): State[LoginData, Unit] = State.modify { s =>
    s.copy(loggedIn = true)
  }
}

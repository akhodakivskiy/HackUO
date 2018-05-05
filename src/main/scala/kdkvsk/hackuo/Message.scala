package kdkvsk.hackuo

import kdkvsk.hackuo.network.RecvPacket

sealed trait Message

object StartupMessage extends Message
object ShutdownMessage extends Message

trait HandlerMessage extends Message

case class PacketMessage(packet: RecvPacket) extends Message

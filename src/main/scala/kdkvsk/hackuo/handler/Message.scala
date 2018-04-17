package kdkvsk.hackuo.handler

import kdkvsk.hackuo.network.{RecvPacket, SendPacket}

sealed trait Message

object ConnectedMessage extends Message
object DisconnectedMessage extends Message

trait HandlerMessage extends Message

case class PacketMessage(packet: RecvPacket) extends Message

case class MultiMessage(messages: Seq[Message]) extends Message

object MultiMessage {
  def apply(message: Message, messages: Message*): MultiMessage = MultiMessage(message +: messages)
}

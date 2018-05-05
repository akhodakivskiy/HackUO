package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

object DenyReason extends Enumeration {
  type Type = Value

  val wrongPassword = Value(0x00, "Incorrect name/password")
  val accountUsed = Value(0x01, "Someone is already using this account")
  val accountBlocked = Value(0x02, "Your account has been blocked")
  val accountCredentialsInvalid = Value(0x03, "Your account credentials are invalid")
  val communicationProblem = Value(0x04, "Communication problem")
  val igrConcurrencyLimit = Value(0x05, "The IGR concurrency limit has been met")
  val igrTimeLimit = Value(0x06, "The IGR time limit has been met")
  val igrAuthFailure = Value(0x07, "General IGR authentication failure.")
}

case class x82_LoginDeniedPacket(reason: DenyReason.Type) extends RecvPacket

object x82_LoginDeniedPacketParser extends RecvPacketParser {
  def packetId: Int = 0x82

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    x82_LoginDeniedPacket(DenyReason.apply(data.readByte()))
  }
}

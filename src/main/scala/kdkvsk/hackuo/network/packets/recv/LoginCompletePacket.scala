package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

object LoginCompletePacket extends RecvPacket {
  val id: Int = LoginConfirmPacketParser.packetId
}

object LoginCompletePacketParser extends RecvPacketParser {
  val packetId: Int = 0x55

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    LoginCompletePacket
  }
}

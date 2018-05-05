package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

object x55_LoginCompletePacket extends RecvPacket {
  val id: Int = x1B_LoginConfirmPacketParser.packetId
}

object x55_LoginCompletePacketParser extends RecvPacketParser {
  val packetId: Int = 0x55

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    x55_LoginCompletePacket
  }
}

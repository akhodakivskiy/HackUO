package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class x6D_PlayMidiMusicPacket(musicId: Short) extends RecvPacket

object x6D_PlayMidiMusicPacketParser extends RecvPacketParser {
  val packetId: Int = 0x6D

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    x6D_PlayMidiMusicPacket(data.readShort())
  }
}

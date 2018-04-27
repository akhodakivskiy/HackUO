package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class PlayMidiMusicPacket(musicId: Short) extends RecvPacket

object PlayMidiMusicPacketParser extends RecvPacketParser {
  val packetId: Int = 0x6D

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    PlayMidiMusicPacket(data.readShort())
  }
}

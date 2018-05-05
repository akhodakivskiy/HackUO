package kdkvsk.hackuo.network.packets.recv

import java.io.DataInputStream

import kdkvsk.hackuo.network.{RecvPacket, RecvPacketParser}

case class CharacterName(index: Byte, name: String, password: String)

case class StartLocation(index: Byte, cityName: String, areaName: String, x: Int, y: Int, z: Int, mapId: Int, clilocInfo: Int, newClient: Boolean)

case class xA9_CharacterListPacket(characters: Seq[CharacterName], locations: Seq[StartLocation], flags: Int, lastCharIdOpt: Option[Int]) extends RecvPacket {
  val id: Int = 0xA9
}

case class xA9_CharacterListPacketParser(newClient: Boolean) extends RecvPacketParser {
  val packetId: Int = 0xA9

  def parse(data: DataInputStream, size: Int): RecvPacket = {
    ensureLength(data, size)

    val charNum: Byte = data.readByte()

    val characters: Array[CharacterName] = Array.fill(charNum)(null)

    Range(0, charNum).foreach { i =>
      val name: String = readStringWithNull(data, 30)
      val password: String = readStringWithNull(data, 30)

      characters(i) = CharacterName(i.byteValue(), name, password)
    }

    val locationNum: Int = data.readByte()

    val locations: Array[StartLocation] = Array.fill(locationNum)(null)

    Range(0, locationNum).foreach { i =>
      if (newClient) {
        val index: Byte = data.readByte()
        val cityName: String = readStringWithNull(data, 32)
        val areaName: String = readStringWithNull(data, 32)
        val x: Int = data.readInt()
        val y: Int = data.readInt()
        val z: Int = data.readInt()
        val mapId: Int = data.readInt()
        val clilocId: Int = data.readInt()
        val _ = data.readInt()

        locations(i) = StartLocation(index, cityName, areaName, x, y, z, mapId, clilocId, newClient = true)
      } else {
        val index: Byte = data.readByte()
        val cityName: String = readStringWithNull(data, 31)
        val areaName: String = readStringWithNull(data, 31)

        locations(i) = StartLocation(index, cityName, areaName, 0, 0, 0, 0, 0, newClient = false)
      }
    }

    val flags: Int = data.readInt()

    xA9_CharacterListPacket(characters, locations, flags, None)
  }
}

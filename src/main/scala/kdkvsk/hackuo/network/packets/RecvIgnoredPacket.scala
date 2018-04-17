package kdkvsk.hackuo.network.packets

import kdkvsk.hackuo.network.RecvPacket

case class RecvIgnoredPacket(id: Int, length: Int, description: String) extends RecvPacket

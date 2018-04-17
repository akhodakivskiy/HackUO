package kdkvsk.hackuo.handler

import kdkvsk.hackuo.network.SendPacket

trait Response

object NoopResponse extends Response
case class TerminateResponse(reason: String) extends Response
case class PacketResponse(packet: SendPacket) extends Response
case class MultiResponse(responses: List[Response]) extends Response

object MultiResponse {
  def apply(response: Response, responses: Response *): Response = MultiResponse(response :: responses.toList)
}



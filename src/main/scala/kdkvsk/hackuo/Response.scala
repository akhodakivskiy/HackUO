package kdkvsk.hackuo

import kdkvsk.hackuo.network.SendPacket

sealed trait Response

object NoopResponse extends Response
case class LogResponse(message: String) extends Response
case class TerminateResponse(reason: String) extends Response
case class MessageResponse(message: Message) extends Response
case class PacketResponse(packet: SendPacket) extends Response
case class MultiResponse(responses: List[Response]) extends Response

object MultiResponse {
  def flatten(response: Response, responses: Response *): Response = {
    flattenResponses(response :: responses.toList) match {
      case Nil => NoopResponse
      case rs => MultiResponse(rs)
    }
  }

  def flattenResponses(responses: List[Response]): List[Response] = {
    responses.foldLeft(List.empty[Response]) {
      case (m, NoopResponse) => m
      case (m, MultiResponse(rs)) => flattenResponses(rs) ::: m
      case (m, r) => r :: m
    }
  }
}



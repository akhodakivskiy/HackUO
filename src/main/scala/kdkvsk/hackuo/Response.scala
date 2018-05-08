package kdkvsk.hackuo

import kdkvsk.hackuo.network.SendPacket

sealed trait Response {
  def combine(response: Response): Response = {
    response match {
      case MultiResponse(rs) => MultiResponse(this :: rs)
      case NoopResponse => this
      case r => MultiResponse(this :: r :: Nil)
    }
  }
}

object NoopResponse extends Response {
  override def combine(response: Response): Response = response
}
case class MultiResponse(responses: List[Response]) extends Response {
  override def combine(response: Response): Response = {
    response match {
      case MultiResponse(rs) => MultiResponse(responses ::: rs)
      case NoopResponse => this
      case r => MultiResponse(r :: responses)
    }
  }
}
case class LogResponse(message: String) extends Response
case class TerminateResponse(reason: String) extends Response
case class MessageResponse(message: Message) extends Response
case class PacketResponse(packet: SendPacket) extends Response
case class EventResponse(event: Event) extends Response

object MultiResponse {
  def combine(responses: Seq[Response]): Response = {
    responses.foldLeft(NoopResponse: Response) {
      case (m, r) => m.combine(r)
    }
  }

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



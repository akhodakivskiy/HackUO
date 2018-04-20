package kdkvsk.hackuo.handler

import cats.data.State
import kdkvsk.hackuo.{Message, MultiResponse, NoopResponse, Response}
import kdkvsk.hackuo.model.World

trait Handler {
  def handle: PartialFunction[Message, State[World, Response]]
}

case class MultiHandler(handlers: List[Handler]) extends Handler {
  val handle: PartialFunction[Message, World.State] = new PartialFunction[Message, World.State] {
    def isDefinedAt(message: Message): Boolean = handlers.exists(_.handle.isDefinedAt(message))
    def apply(message: Message): World.State = State { world =>
      val init: (World, Response) = (world, NoopResponse)

      handlers.filter(_.handle.isDefinedAt(message)).foldLeft(init) {
        case ((w, r), handler) if !handler.handle.isDefinedAt(message) => (w, r)
        case ((w, MultiResponse(rs)), handler) => handler.handle.apply(message).map(r => MultiResponse(r :: rs)).run(w).value
        case ((w, NoopResponse), handler) => handler.handle.apply(message).run(w).value
        case ((w, r), handler) => handler.handle.apply(message).map(r2 => MultiResponse(r2 :: r :: Nil)).run(w).value
      }
    }
  }
}

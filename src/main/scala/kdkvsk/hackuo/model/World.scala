package kdkvsk.hackuo.model

import kdkvsk.hackuo.handler._
import kdkvsk.hackuo.model.common.Serial

case class World(login: LoginState,
                 mapId: Int = 0,
                 playerSerial: Serial = Serial(0),
                 mobiles: Map[Serial, Mobile] = Map.empty,
                 objects: Map[Serial, ObjectItem] = Map.empty,
                 multis: Map[Serial, MultiItem] = Map.empty,
                 movement: Movement = Movement(),

                 loginHandlerOpt: Option[LoginHandler] = None,
                 handlers: List[Handler] = Nil) {
  lazy val player: Mobile = mobiles.getOrElse(playerSerial, throw new IllegalStateException(s"player mobile [$playerSerial] not registered"))

  def modifyPlayer(mod: Mobile => Mobile): World = {
    mobiles.get(playerSerial).map(p => copy(mobiles = mobiles.updated(p.serial, mod(p)))).getOrElse(this)
  }

  def modifyMovement(mod: Movement => Movement): World = {
    copy(movement = mod(movement))
  }

  lazy val handler: Handler = MultiHandler(MovementHandler :: ItemHandler :: MobileHandler :: loginHandlerOpt.toList ::: handlers)
}

object World {
  type State = cats.data.State[World, Response]

  def modify(f: World => World): State = cats.data.State { w =>
    (f(w), NoopResponse)
  }

  def modifyIf(condition: World => Boolean)(mod: World => World): State = cats.data.State { w =>
    if (condition(w)) {
      (mod(w), NoopResponse)
    } else {
      (w, NoopResponse)
    }
  }

  def stateIf(condition: World => Boolean)(stateF: World => (World, Response)): State = {
    stateIfElse(condition)(cats.data.State.apply(stateF))(cats.data.State.pure(NoopResponse))
  }

  def stateIfElse(condition: World => Boolean)(a: State)(b: State): State = {
    cats.data.State.inspect(condition).flatMap {
      case true => a
      case false => b
    }
  }

  val noopState: State = cats.data.State.pure(NoopResponse)
}
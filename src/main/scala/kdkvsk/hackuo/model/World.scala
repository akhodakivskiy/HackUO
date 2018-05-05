package kdkvsk.hackuo.model

import kdkvsk.hackuo.client.Client
import kdkvsk.hackuo.model.common.Serial
import kdkvsk.hackuo.{NoopResponse, Response}

case class World(client: Client,
                 login: LoginState,
                 mapId: Int = 0,
                 playerSerial: Serial = Serial(0),

                 mobiles: Map[Serial, Mobile] = Map.empty,
                 item2mobile: Map[Serial, Serial] = Map.empty,

                 containers: Map[Serial, Container] = Map.empty,
                 item2container: Map[Serial, Serial] = Map.empty,

                 objects: Map[Serial, ObjectItem] = Map.empty,
                 multis: Map[Serial, MultiItem] = Map.empty,

                 popupOpt: Option[Popup] = None,
                 movement: Movement = Movement()) {
  lazy val player: Mobile = mobiles.getOrElse(playerSerial, throw new IllegalStateException(s"player mobile [$playerSerial] not registered"))

  def modifyPlayer(mod: Mobile => Mobile): World = {
    mobiles.get(playerSerial).map(p => copy(mobiles = mobiles.updated(p.serial, mod(p)))).getOrElse(this)
  }

  def modifyMovement(mod: Movement => Movement): World = {
    copy(movement = mod(movement))
  }
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
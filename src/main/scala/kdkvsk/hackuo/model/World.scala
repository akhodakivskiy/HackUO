package kdkvsk.hackuo.model

import cats.data.State
import kdkvsk.hackuo.client.Client
import kdkvsk.hackuo.lib.IntValue
import kdkvsk.hackuo.model.common.Serial
import kdkvsk.hackuo.network.packets.send.x06_DoubleClickPacket
import kdkvsk.hackuo.model.composition._
import kdkvsk.hackuo.{composition, _}

case class World(client: Client,

                 login: LoginData,
                 mobiles: MobilesData = MobilesData(Serial(0), Map.empty, Map.empty),
                 containers: ContainerData = ContainerData(Map.empty, Map.empty),
                 map: MapData = MapData(0, Map.empty, Map.empty),

                 interaction: InteractionData = InteractionData(None, Map.empty),
                 movement: MovementData = MovementData()) extends Part[World] {
  def movePlayer: (MovementData, Mobile) = (movement, player)
  def setMovePlayer(a: (MovementData, Mobile)): World = copy(movement = a._1, mobiles = mobiles.copy(mobiles = mobiles.mobiles.updated(a._2.serial, a._2)))

  def player: Mobile = mobiles.mobiles.getOrElse(mobiles.playerSerial, throw new IllegalStateException(s"player mobile [${mobiles.playerSerial}] not registered"))
}

object World extends PartOps[Unit, World] {
  def empty(client: Client, loginData: LoginData): World = World(client, loginData)

  val packetHandler: PartialFunction[Message, State[World, Response]] = {
    case REPLInput(command) =>  State.inspect(w => handleREPLCommand(w, command))
  }

  val handler: PartialFunction[Message, State[World, Response]] = {
    PartOps.combine(
      packetHandler,
      LoginData.worldHandler,
      ContainerData.worldHandler,
      MobilesData.worldHandler,
      InteractionData.worldHandler,
      MovementData.worldHandler,
      MapData.worldHandler
    )
  }

  def handleREPLCommand(w: World, command: List[String]): Response = {
    command match {
      case ("exit" | "quit" | "q") :: Nil => TerminateResponse("terminated by user")
      case "walk" :: IntValue(dx) :: IntValue(dy) :: Nil => MessageResponse(StartMoveInput(dx, dy, isRunning = false, 0))
      case "run" :: IntValue(dx) :: IntValue(dy) :: Nil => MessageResponse(StartMoveInput(dx, dy, isRunning = true, 0))
      case "syncpos" :: Nil => MessageResponse(SyncPositionInput)
      case "paperdoll" :: Nil => PacketResponse(x06_DoubleClickPacket(w.player.serial.value))
      case "backpack" :: Nil => PacketResponse(x06_DoubleClickPacket(w.player.backpackOpt.getOrElse(throw new IllegalStateException(s"player doesn't have a backpack")).serial.value))
      case "click" :: IntValue(serial) :: Nil => MessageResponse(SingleClickInput(Serial(serial)))
      case "doubleclick" :: IntValue(serial) :: Nil => MessageResponse(DoubleClickInput(Serial(serial)))
      case "popup" :: IntValue(serial) :: Nil => MessageResponse(RequestPopupInput(Serial(serial)))
      case "popup" :: "self" :: Nil => MessageResponse(RequestPopupInput(w.player.serial))
      case "popup" :: "respond" :: IntValue(index) :: Nil => MessageResponse(SelectPopupEntryInput(index))
      case "cliloc" :: IntValue(serial) :: Nil => MessageResponse(RequestClilocInput(Serial(serial) :: Nil))
      case _ => LogResponse("unrecognized command")
    }
  }

  def get(w: Unit): World = throw new IllegalStateException("should not arrive here")

  def set(w: Unit, t: World): Unit = throw new IllegalArgumentException("should not arrive here")
}
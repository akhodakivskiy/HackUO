package kdkvsk.hackuo.handler
import cats.data.State
import com.typesafe.scalalogging.LazyLogging
import kdkvsk.hackuo.client.map.LandTile
import kdkvsk.hackuo.model._
import kdkvsk.hackuo.model.common.Direction
import kdkvsk.hackuo.network.packets.recv.{x22_MoveAckPacket, x21_MoveRejPacket}
import kdkvsk.hackuo.network.packets.recvsend.{xBF_2_FastWalkPreventionAddKeyPacket, xBF_1_FastWalkPreventionInitPacket}
import kdkvsk.hackuo.network.packets.send.x02_MoveRequestPacket
import kdkvsk.hackuo.{HandlerMessage, Message, PacketMessage, PacketResponse}

case class StartMoveHandlerMessage(dx: Int, dy: Int, isRunning: Boolean, retryCount: Int) extends HandlerMessage
object StopMoveHandlerMessage extends HandlerMessage
object SyncMoveHandlerMessage extends HandlerMessage

object MovementHandler extends Handler with MoveHandlerOps {
  val handle: PartialFunction[Message, World.State] = {
    //case PacketMessage(LoginCompletePacket) => initFastKeys
    case PacketMessage(p: xBF_1_FastWalkPreventionInitPacket) => fastWalkInit(p)
    case PacketMessage(p: xBF_2_FastWalkPreventionAddKeyPacket) => fastWalkAddKey(p)
    case PacketMessage(p: x22_MoveAckPacket) => moveAccept(p)
    case PacketMessage(p: x21_MoveRejPacket) => moveReject(p)
    case StartMoveHandlerMessage(dx, dy, isRunning, retryCount) => startMove(dx, dy, isRunning, retryCount)
    case StopMoveHandlerMessage => stopMove
    case SyncMoveHandlerMessage => syncRequest
  }
}

trait MoveHandlerOps extends LazyLogging {
  val initFastKeys: World.State = State.pure {
    val keys: List[Int] = List.iterate[Int](1, 6)(_ + 1)
    PacketResponse(xBF_1_FastWalkPreventionInitPacket(keys))
  }

  def fastWalkInit(p: xBF_1_FastWalkPreventionInitPacket): World.State = World.modify { w =>
    w.copy(movement = w.movement.copy(fastWalkStack = p.keys))
  }

  def fastWalkAddKey(p: xBF_2_FastWalkPreventionAddKeyPacket): World.State = World.modify { w =>
    w.copy(movement = w.movement.copy(fastWalkStack = p.key :: w.movement.fastWalkStack))
  }

  def moveAccept(p: x22_MoveAckPacket): World.State = {
    World.stateIfElse(_.movement.isEnabled) {
      State.modify[World] { w =>
        val moveDirection: Direction.Type = Point2D.direction(w.player, w.movement)
        if (w.player.direction == moveDirection) {
          val (newX, newY) = Point2D.pointInDirection(w.player, w.player.direction)

          val tile: LandTile = w.client.maps(0).tileMatrix.getLandTile(newX, newY)
          logger.info(s"tile: $tile")

          w.modifyPlayer(_.copy(x = newX, y = newY))
        } else {
          w.modifyPlayer(_.copy(direction = moveDirection))
        }
      }.flatMap { _ =>
        World.stateIfElse(w => Point2D.isSame(w.player, w.movement))(stopMove)(moveRequest)
      }
    } {
      syncRequest
    }
  }

  def moveReject(p: x21_MoveRejPacket): World.State = {
    for {
      _ <- World.modify(_.modifyMovement(m => m.copy(retryCount = m.retryCount - 1, sequence = 0)))
      r <- World.stateIfElse(_.movement.retryCount <= 0)(stopMove)(moveRequest)
    } yield {
      r
    }
  }

  val incrementSequence: State[World, Int] = State { w =>
    val sequence: Int = w.movement.sequence
    val nextSequence: Int = if (sequence >= 255) 1 else sequence + 1

    (w.copy(movement = w.movement.copy(sequence = nextSequence)), sequence)
  }

  val popFastWalkKey: State[World, Int] = State { w =>
    if (w.movement.fastWalkStack.isEmpty) {
      logger.trace(s"fast walk stack is empty")
      (w, 0)
    } else {
      val key :: tailKeys = w.movement.fastWalkStack

      (w.copy(movement = w.movement.copy(fastWalkStack = tailKeys)), key)
    }
  }

  val moveRequest: World.State = {
    World.stateIfElse(w => Point2D.isSame(w.player, w.movement)) {
      World.modify(_.modifyMovement(_.copy(isEnabled = false)))
    } {
      for {
        player <- State.inspect[World, Mobile](_.player)
        ms <- State.inspect[World, Movement](_.movement)
        direction = Point2D.direction(player, ms)
        sequence <- incrementSequence
        fastKey <- popFastWalkKey
      } yield {
        PacketResponse(x02_MoveRequestPacket(direction, ms.isRunning, sequence, fastKey))
      }
    }
  }

  def setTarget(x: Int, y: Int, isRunning: Boolean, retryCount: Int): World.State = {
    World.modify(_.modifyMovement(_.copy(x = x, y = y, isRunning = isRunning, retryCount = retryCount, isEnabled = true)))
  }

  def startMove(dx: Int, dy: Int, isRunning: Boolean, retryCount: Int): World.State = {
    for {
      player <- State.inspect[World, Mobile](_.player)
      _ <- setTarget(player.x + dx, player.y + dy, isRunning, retryCount)
      r <- moveRequest
    } yield {
      r
    }
  }

  val stopMove: World.State = World.modify { w =>
    w.copy(movement = w.movement.copy(isEnabled = false))
  }

  val syncRequest: World.State = {
    for {
      sequence <- incrementSequence
      notoriety <- State.inspect[World, Notoriety.Type](_.player.notoriety)
    } yield {
      PacketResponse(x22_MoveAckPacket(sequence.byteValue(), notoriety))
    }
  }
}

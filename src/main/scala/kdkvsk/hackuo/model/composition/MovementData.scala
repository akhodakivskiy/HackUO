package kdkvsk.hackuo.model.composition

import cats.data.State
import com.typesafe.scalalogging.LazyLogging
import kdkvsk.hackuo.client.map.LandTile
import kdkvsk.hackuo.model.common.Direction
import kdkvsk.hackuo._
import kdkvsk.hackuo.model.{Mobile, Notoriety, Point2D, World}
import kdkvsk.hackuo.network.packets.recv.{x21_MoveRejPacket, x22_MoveAckPacket, x55_LoginCompletePacket}
import kdkvsk.hackuo.network.packets.recvsend.{xBF_1_FastWalkPreventionInitPacket, xBF_2_FastWalkPreventionAddKeyPacket}
import kdkvsk.hackuo.network.packets.send.x02_MoveRequestPacket

case class MovementData(x: Int = 0,
                        y: Int = 0,
                        isRunning: Boolean = false,
                        sequence: Int = 0,
                        fastWalkStack: List[Int] = Nil,
                        isEnabled: Boolean = false,
                        retryCount: Int = 0) extends Part[MovementData] with Point2D

object MovementData extends PartOps[World, (MovementData, Mobile)] with LazyLogging {
  def get(w: World): (MovementData, Mobile) = (w.movement, w.player)
  def set(w: World, t: (MovementData, Mobile)): World = {
    w.copy(movement = t._1, mobiles = w.mobiles.copy(mobiles = w.mobiles.mobiles.updated(t._2.serial, t._2)))
  }

  val initFastKeys: State[(MovementData, Mobile), Response] = State.pure {
    val keys: List[Int] = List.iterate[Int](1, 6)(_ + 1)
    PacketResponse(xBF_1_FastWalkPreventionInitPacket(keys))
  }

  val stopMove: State[(MovementData, Mobile), Unit] = State.modify { case (w, m) =>
    (w.copy(isEnabled = false), m)
  }

  def fastWalkInit(p: xBF_1_FastWalkPreventionInitPacket): State[(MovementData, Mobile), Unit] = State.modify { case (w, m) =>
    (w.copy(fastWalkStack = p.keys), m)
  }

  def fastWalkAddKey(p: xBF_2_FastWalkPreventionAddKeyPacket): State[(MovementData, Mobile), Unit] = State.modify { case (w, m) =>
    (w.copy(fastWalkStack = p.key :: w.fastWalkStack), m)
  }

  def moveAccept(p: x22_MoveAckPacket): State[(MovementData, Mobile), Response] = {
    State.inspect[(MovementData, Mobile), Boolean](_._1.isEnabled).flatMap {
      case false => syncRequest
      case true =>
        State.modify[(MovementData, Mobile)] { case (w, m) =>
          val moveDirection: Direction.Type = Point2D.direction(m, w)
          if (m.direction == moveDirection) {
            val (newX, newY) = Point2D.pointInDirection(m, m.direction)

            (w, m.copy(x = newX, y = newY))
          } else {
            (w, m.copy(direction = moveDirection))
          }
        }.flatMap { _ =>
          moveRequest
        }
    }
  }

  def moveReject(p: x21_MoveRejPacket): State[(MovementData, Mobile), Response] = {
    State.apply[(MovementData, Mobile), Int] {case (w, m) =>
      ((w.copy (retryCount = w.retryCount + 1), m), w.retryCount)
    }.flatMap {
      case n if n > 0 => moveRequest
      case _ => stopMove
    }
  }


  val incrementSequence: State[(MovementData, Mobile), Int] = State { case (w, m) =>
    val sequence: Int = w.sequence
    val nextSequence: Int = if (sequence >= 255) 1 else sequence + 1

    ((w.copy(sequence = nextSequence), m), sequence)
  }

  val popFastWalkKey: State[(MovementData, Mobile), Int] = State { case (w, m) =>
    if (w.fastWalkStack.isEmpty) {
      logger.trace(s"fast walk stack is empty")
      ((w, m), 0)
    } else {
      val key :: tailKeys = w.fastWalkStack
      ((w.copy(fastWalkStack = tailKeys), m), key)
    }
  }

  val moveRequest: State[(MovementData, Mobile), Response] = {
    State.inspect[(MovementData, Mobile), Boolean](Point2D.isSame).flatMap {
      case true => stopMove
      case false =>
        for {
          ms <- State.inspect[(MovementData, Mobile), MovementData](_._1)
          direction <- State.inspect[(MovementData, Mobile), Direction.Type](Point2D.direction)
          sequence <- incrementSequence
          fastKey <- popFastWalkKey
        } yield {
          PacketResponse(x02_MoveRequestPacket(direction, ms.isRunning, sequence, fastKey))
        }
    }
  }

  def setTarget(x: Int, y: Int, isRunning: Boolean, retryCount: Int): State[(MovementData, Mobile), Unit] = State.modify { case (w, m) =>
    (w.copy(x = x, y = y, isRunning = isRunning, retryCount = retryCount, isEnabled = true), m)
  }

  def startMove(dx: Int, dy: Int, isRunning: Boolean, retryCount: Int): State[(MovementData, Mobile), Response] = {
    for {
      player <- State.inspect[(MovementData, Mobile), Mobile](_._2)
      _ <- setTarget(player.x + dx, player.y + dy, isRunning, retryCount)
      r <- moveRequest
    } yield {
      r
    }
  }

  val syncRequest: State[(MovementData, Mobile), Response] = {
    for {
      sequence <- incrementSequence
      notoriety <- State.inspect[(MovementData, Mobile), Notoriety.Type](_._2.notoriety)
    } yield {
      PacketResponse(x22_MoveAckPacket(sequence.byteValue(), notoriety))
    }
  }

  val handler: PartialFunction[Message, State[(MovementData, Mobile), Response]] = {
    case PacketMessage(_: x55_LoginCompletePacket.type) => initFastKeys
    case PacketMessage(p: xBF_1_FastWalkPreventionInitPacket) => fastWalkInit(p)
    case PacketMessage(p: xBF_2_FastWalkPreventionAddKeyPacket) => fastWalkAddKey(p)
    case PacketMessage(p: x22_MoveAckPacket) => moveAccept(p)
    case PacketMessage(p: x21_MoveRejPacket) => moveReject(p)
    case StartMoveInput(dx, dy, isRunning, retryCount) => startMove(dx, dy, isRunning, retryCount)
    case StopMoveInput => stopMove
    case SyncPositionInput => syncRequest
  }
}

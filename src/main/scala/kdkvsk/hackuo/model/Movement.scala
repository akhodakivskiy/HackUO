package kdkvsk.hackuo.model

import kdkvsk.hackuo.model.common.Direction

case class Movement(x: Int = 0,
                    y: Int = 0,
                    isRunning: Boolean = false,
                    sequence: Int = 0,
                    fastWalkStack: List[Int] = Nil,
                    isEnabled: Boolean = false,
                    retryCount: Int = 0) extends Point2D

object Movement {
  type State[T] = cats.data.State[Movement, T]
}

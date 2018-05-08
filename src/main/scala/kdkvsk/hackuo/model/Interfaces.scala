package kdkvsk.hackuo.model

import kdkvsk.hackuo.model.common.Direction

trait Point2D {
  def x: Int
  def y: Int
}

object Point2D {
  def isSame(pts: (Point2D, Point2D)): Boolean = {
    isSame(pts._1, pts._2)
  }

  def isSame(p1: Point2D, p2: Point2D): Boolean = {
    p1.x == p2.x && p1.y == p2.y
  }

  def isSame(p1: Point2D, x: Int, y: Int): Boolean = {
    p1.x == x && p1.y == y
  }

  def direction(pts: (Point2D, Point2D)): Direction.Type = {
    direction(pts._1, pts._2)
  }

  def direction(from: Point2D, to: Point2D): Direction.Type = {
    direction(from, to.x, to.y)
  }

  def direction(from: Point2D, x: Int, y: Int): Direction.Type = {
    val dx: Int = x - from.x
    val dy: Int = y - from.y

    val adx: Int = Math.abs(dx)
    val ady: Int = Math.abs(dy)

    if (adx >= ady * 3) {
      if (dx > 0) Direction.East else Direction.West
    } else if (ady >= adx * 3) {
      if (dy > 0) Direction.South else Direction.North
    } else if (dx > 0) {
      if (dy > 0) Direction.Down else Direction.Right
    } else {
      if (dy > 0) Direction.Left else Direction.Up
    }
  }

  def pointInDirection(from: Point2D, direction: Direction.Type): (Int, Int) = {
    direction match {
      case Direction.North => (from.x,      from.y - 1)
      case Direction.Right => (from.x + 1,  from.y - 1)
      case Direction.East =>  (from.x + 1,  from.y)
      case Direction.Down =>  (from.x + 1,  from.y + 1)
      case Direction.South => (from.x,      from.y + 1)
      case Direction.Left =>  (from.x - 1,  from.y + 1)
      case Direction.West =>  (from.x - 1,  from.y)
      case Direction.Up =>    (from.x - 1,  from.y - 1)
    }
  }
}

trait Point3D extends Point2D {
  def z: Int
}

package kdkvsk.hackuo.client

import java.util.concurrent.ArrayBlockingQueue

import com.typesafe.scalalogging.LazyLogging

class QueueIterable[T](queue: ArrayBlockingQueue[T], terminalOpt: Option[T] = None) extends Iterable[T] with LazyLogging {
  def iterator: Iterator[T] = new Iterator[T] {
    private var seenTerminal: Boolean = false
    private var queueHead: Option[T] = None

    def hasNext: Boolean = {
      if (queueHead.isDefined) {
        true
      } else if (seenTerminal) {
        false
      } else {
        val head: T = queue.take()

        if (terminalOpt.contains(head)) {
          seenTerminal = true
        }
        queueHead = Some(head)

        true
      }
    }

    def next(): T = {
      val result: T = queueHead.get
      queueHead = None
      result
    }

    override def toString(): String = {
      s"queue to message iterator $queueHead"
    }
  }

  override def toString(): String = {
    "queue to message iterable"
  }
}

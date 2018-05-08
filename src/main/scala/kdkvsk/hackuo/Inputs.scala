package kdkvsk.hackuo

import kdkvsk.hackuo.model.common.Serial

case class REPLInput(command: List[String]) extends InputMessage

case class StartMoveInput(dx: Int, dy: Int, isRunning: Boolean, retryCount: Int) extends InputMessage
object StopMoveInput extends InputMessage
object SyncPositionInput extends InputMessage

case class SingleClickInput(serial: Serial) extends InputMessage
case class DoubleClickInput(serial: Serial) extends InputMessage
case class RequestPopupInput(serial: Serial) extends InputMessage
case class SelectPopupEntryInput(index: Int) extends InputMessage
case class RequestClilocInput(serials: Seq[Serial]) extends InputMessage

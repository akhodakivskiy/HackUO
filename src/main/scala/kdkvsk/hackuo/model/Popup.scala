package kdkvsk.hackuo.model

import kdkvsk.hackuo.model.common.Serial

case class PopupEntry(index: Int, textId: Int, flags: Int, hueOpt: Option[Int])

case class Popup(serial: Serial, entries: Seq[PopupEntry])

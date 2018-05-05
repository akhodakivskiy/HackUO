package kdkvsk.hackuo.model.common

object ClientFlag extends Enumeration {
  type Type = Value

  val T2a: ValueSet = ValueSet.empty

  val Renaissance: Value = Value(0, "renaissance")
  val ThirdDawn: Value = Value(1, "third dawn")
  val Lbr: Value = Value(2, "lbr")
  val Aos: Value = Value(3, "aos")
  val Se: Value = Value(4, "se")
  val Sa: Value = Value(5, "sa")
  val Uo3d: Value = Value(6, "uo3d")
  val Reserved: Value = Value(7, "reserved")
  val Client3D: Value = Value(8, "3d")

  val Latest: ValueSet = Renaissance + ThirdDawn + Lbr + Aos + Se + Sa
}


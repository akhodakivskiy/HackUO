package kdkvsk.hackuo.client.map

import org.scalatest.{FunSuite, Matchers}

class TileDataTest extends FunSuite with Matchers {
  test("tile flags") {

    val s = TileFlag.ValueSet.fromBitMask(Array(0x00040001))
    s should contain (TileFlag.PartialHue)
    s should contain (TileFlag.Background)
  }
}

package kdkvsk.hackuo.model.common

import org.scalatest.{FunSuite, Matchers}

class ClientFlagTest extends FunSuite with Matchers {
  test("test flags") {
    ClientFlag.Latest.toBitMask.head shouldBe 0x3F
  }
}

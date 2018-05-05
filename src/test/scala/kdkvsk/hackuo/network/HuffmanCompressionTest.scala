package kdkvsk.hackuo.network

import kdkvsk.hackuo.client.compression.HuffmanCompression
import org.scalatest.{FunSuite, Matchers}

class HuffmanCompressionTest extends FunSuite with Matchers {
  test("decompression") {
    val comp = new HuffmanCompression()

    val in: Array[Byte] = Array(0xB3, 0x24, 0xCD, 0xC6, 0x80).map(_.byteValue())
    val out: Array[Byte] = Array.fill(10)(0x0)

    val result = comp.decompress(in, 0, in.length, out, 0)

    result.inSize shouldBe 5
    result.outSize shouldBe 3
    result.seenBoundary shouldBe true

    (out(0) & 0xFF) shouldBe 0xB9
    (out(1) & 0xFF) shouldBe 0x82
    (out(2) & 0xFF) shouldBe 0xFB
  }
}

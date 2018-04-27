package kdkvsk.hackuo.lib.map

case class LandTile(id: Int, z: Byte)

case class TileMatrix(landTiles: Array[Array[Array[LandTile]]],
                      uopIndex: UopIndex,
                      width: Int,
                      height: Int,
                      fileIndex: Int) {
  val blockWidth: Int = width >>> 3
  val blockHeight: Int = height >>> 3
}

object TileMatrix {
}

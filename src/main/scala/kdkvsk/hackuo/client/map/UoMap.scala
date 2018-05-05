package kdkvsk.hackuo.client.map

import java.nio.file.Path

class UoMap(val name: String, val tileMatrix: TileMatrix, tileData: TileData) {
  def landTile(x: Int, y: Int): LandTile = tileMatrix.getLandTile(x, y)
  def staticTiles(x: Int, y: Int): Seq[StaticTile] = tileMatrix.getStaticTiles(x, y)
}

object UoMap {
}

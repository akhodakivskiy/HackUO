package kdkvsk.hackuo.client

import java.nio.file.{Files, Path}

import kdkvsk.hackuo.client.map.{TileData, TileMatrix, UoMap}

import scala.collection.JavaConverters._
import scala.collection.mutable

class Client(path: Path, language: String) {
  val configUo: ClientConfig = ClientConfig.load(Client.resolveFile(path,"Uo.cfg"))
  val configLogin: ClientConfig = ClientConfig.load(Client.resolveFile(path,"Login.cfg"))
  val cliloc: Cliloc = Cliloc.load(Client.resolveFile(path,s"Cliloc.$language"))
  val tileData: TileData = TileData.load(Client.resolveFile(path,"tiledata.mul"))
  val maps: Map[Int, UoMap] = Client.resolveAllMaps(path, tileData)
}

object Client {
  def resolveFile(clientDir: Path, fileName: String): Path = {
    resolveFileOpt(clientDir, fileName).getOrElse(throw new IllegalArgumentException(s"can't find $fileName in $clientDir"))
  }

  def resolveFileOpt(clientDir: Path, fileName: String): Option[Path] = {
    Files.list(clientDir).iterator().asScala.find(_.getFileName.toString.compareToIgnoreCase(fileName) == 0)
  }

  def resolveMap(clientDir: Path, tileData: TileData, name: String, fileIndex: Int, width: Int, height: Int): Option[UoMap] = {
    val staticIndexPathOpt: Option[Path] = resolveFileOpt(clientDir, s"staidx$fileIndex.mul")
    val staticPathOpt: Option[Path] = resolveFileOpt(clientDir, s"statics$fileIndex.mul")
    val mapPathOpt: Option[(Path, Boolean)] = Client.resolveFileOpt(clientDir, s"map${fileIndex}LegacyMUL.uop").map {
      mapUopPath => mapUopPath -> true
    }.orElse {
      Client.resolveFileOpt(clientDir, s"map$fileIndex.mul").map { mapPath =>
        mapPath -> false
      }
    }

    for {
      staticIndexPath <- staticIndexPathOpt
      staticPath <- staticPathOpt
      (mapPath, withMapIndex) <- mapPathOpt
    } yield {
      new UoMap(name, new TileMatrix(fileIndex, width, height, mapPath, withMapIndex = true, staticIndexPath, staticPath), tileData)
    }
  }

  def resolveAllMaps(clientDir: Path, tileData: TileData): Map[Int, UoMap] = {
    Seq(
      resolveMap(clientDir, tileData, "Felucca", 0, 7168, 4096),
      resolveMap(clientDir, tileData, "Trammel", 1, 7168, 4096),
      resolveMap(clientDir, tileData, "Ilshenar", 2, 2304, 1600),
      resolveMap(clientDir, tileData, "Malas", 3, 2560, 2048),
      resolveMap(clientDir, tileData, "Tokuno", 4, 1448, 1448),
      resolveMap(clientDir, tileData, "TerMur", 5, 1280, 4096)
    ).flatten.map { m =>
      m.tileMatrix.fileIndex -> m
    }.toMap
  }
}

package kdkvsk.hackuo.client

import java.io.DataInputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import com.typesafe.scalalogging.LazyLogging

import scala.collection.mutable

case class ClilocEntry(number: Int, flag: Byte, text: String)

case class Cliloc(entries: Map[Int, ClilocEntry]) {
  def getAndReplace(clilocId: Int, args: String): Option[String] = {
    entries.get(clilocId).map { entry =>
      Cliloc.substitute(clilocId, entry.text, args)
    }
  }
}

object Cliloc extends LazyLogging {
  import kdkvsk.hackuo.lib.DataInputUtils.DataInputOps

  def substitute(textId: Int, text: String, args: String): String = {
    args.split('\t').filter(_.nonEmpty).foldLeft(text) {
      case (t, arg) =>
        val i1: Int = t.indexOf('~')
        val i2: Int = t.indexOf('~', i1 + 1)

        if (i1 >= 0 && i2 > 0) {
          val start: String = t.substring(0, i1 - 1)
          val end: String = t.substring(i2 + 1, t.length - 1)

          start + arg + end
        } else {
          logger.warn(s"no placeholder for argument '$arg' in cliloc $textId '$text'")
          t
        }
    }
  }

  def load(path: Path): Cliloc = {
    logger.info(s"loading cliloc from $path")

    val data: Array[Byte] = new Array[Byte](1024)

    val entries: mutable.Map[Int, ClilocEntry] = mutable.Map.empty

    if (!Files.exists(path)) {
      throw new IllegalArgumentException(s"cliloc file $path doesn't exist")
    }

    entries.clear()

    val dis: DataInputStream = new DataInputStream(Files.newInputStream(path))

    val header1: Int = dis.readInt()
    val header2: Short = dis.readShort()

    while (dis.available() > 0) {
      val number: Int = dis.readIntLE()
      val flag: Byte = dis.readByte()
      val length: Int = dis.readShortLE()

      val buffer: Array[Byte] = if (length > data.length) {
        new Array[Byte](length)
      } else {
        data
      }

      dis.read(buffer, 0, length)
      val text: String = new String(buffer, 0, length, StandardCharsets.UTF_8)

      val entry: ClilocEntry = ClilocEntry(number, flag, text)

      entries.put(number, entry)
    }

    Cliloc(entries.toMap)
  }
}

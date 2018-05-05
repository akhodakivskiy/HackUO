package kdkvsk.hackuo.client

import java.net.InetAddress
import java.nio.file.{Files, Path}

import com.typesafe.scalalogging.LazyLogging

import scala.collection.JavaConverters._

case class ClientConfig(path: Path, values: Map[String, Set[String]]) {
  def valueOpt(name: String): Option[String] = values.get(name).flatMap(_.headOption)
  def value(name: String, defaultValue: String): String = valueOpt(name).getOrElse(defaultValue)
}

object ClientConfig extends LazyLogging {
  def unapply(line: String): Option[(String, String)] = {
    line.split("=").toList.map(_.trim).filter(_.nonEmpty) match {
      case name :: value :: Nil => Some((name, value))
      case name :: Nil => Some((name, ""))
      case _ => None
    }
  }

  def load(path: Path): ClientConfig = {
    logger.info(s"loading config from $path")

    if (!Files.exists(path)) {
      throw new IllegalArgumentException(s"config file $path does not exist")
    }

    val reader = Files.newBufferedReader(path)

    reader.lines().iterator().asScala.map(_.trim).foldLeft(ClientConfig(path, Map.empty)) {
      case (c, line) if line.isEmpty => c
      case (c, line) if line.startsWith(";") => c
      case (c, ClientConfig(name, value)) => c.copy(values = c.values.updated(name, c.values.getOrElse(name, Set.empty) + value))
      case (c, line) =>
        logger.warn(s"failed to parse config entry $line")
        c
    }
  }

  def loginServerOpt(config: ClientConfig): Option[(InetAddress, Int)] = {
    config.valueOpt("LoginServer").flatMap { value =>
      value.split(",").toList match {
        case host :: IntValue(port) :: Nil => Some(InetAddress.getByName(host) -> port)
        case _ => None
      }
    }
  }

  def nextLoginKey(config: ClientConfig): Int = {
    config.value("NextLoginKey", "0").toInt
  }
}

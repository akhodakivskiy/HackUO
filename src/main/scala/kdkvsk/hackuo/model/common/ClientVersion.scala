package kdkvsk.hackuo.model.common

import kdkvsk.hackuo.lib.IntValue
import scopt.Read

case class ClientVersion(major: Int, minor: Int, revision: Int, patch: Int) {
  val stringify: String = s"$major.$minor.$revision.$patch"

  def keys: (Int, Int) = ClientVersion.calculateKeys(this)
}

object ClientVersion {
  def fromStringOpt(value: String): Option[ClientVersion] = {
    value.split("\\.").toList match {
      case IntValue(major) :: IntValue(minor) :: IntValue(revision) :: IntValue(patch) :: Nil =>
        Some(ClientVersion(major, minor, revision, patch))
      case _ =>
        None
    }
  }

  def fromString(value: String):ClientVersion = fromStringOpt(value).getOrElse(throw new IllegalArgumentException(s"failed to parse ClientVersion from $value"))

  implicit val clientVersionReader: Read[ClientVersion] = Read.reads[ClientVersion](ClientVersion.fromString)

  def calculateKeys(version: ClientVersion): (Int, Int) = {
    val maj: Int = version.major
    val min: Int = version.minor
    val rev: Int = version.revision
    val _: Int = version.patch // not used

    var key1: Int = ((maj << 0x17) | (min << 14)) | (rev << 4)
    key1 ^= (rev * rev) << 9
    key1 ^= (min * min)
    key1 ^= ((min * 11) << 0x18)
    key1 ^= ((rev * 7) << 0x13)
    key1 ^= 0x2c13a5fd

    var key2 = ((maj << 0x16) | (rev << 13)) | (min << 3)
    key2 ^= (((rev * rev) * 3) << 10)
    key2 ^= min * min
    key2 ^= ((min * 13) << 0x17)
    key2 ^= ((rev * 7) << 0x12)
    key2 ^= 0xa31d527f

    (key1, key2)
  }
}

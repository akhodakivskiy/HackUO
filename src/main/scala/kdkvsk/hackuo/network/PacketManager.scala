package kdkvsk.hackuo.network

import java.io._
import java.net.Socket
import java.nio.ByteBuffer

import com.typesafe.scalalogging.LazyLogging
import kdkvsk.hackuo.lib.compression.{Compression, NoCompression}
import kdkvsk.hackuo.lib.crytpo.{Crypto, NoCrypto}
import kdkvsk.hackuo.network.packets.recv._
import kdkvsk.hackuo.network.packets.recvsend.{ClientVersionPacketParser, GeneralInfoPacketParser, WarModePacketParser}

class PacketManager(socket: Socket) extends LazyLogging {
  private var compression: Compression = new NoCompression()
  private var logPackets: Boolean = true
  private var crypto: Crypto = new NoCrypto()

  def isConnected: Boolean = socket.isConnected
  def isClosed: Boolean = socket.isClosed

  def setCrypto(crypto: Crypto): Unit = {
    this.crypto = crypto
  }

  def setCompression(compression: Compression): Unit = {
    this.compression = compression;
  }

  def setLogPackets(value: Boolean): Unit = {
    logPackets = value
  }

  private val packetParsers: Array[RecvPacketParser] = Array.fill(256)(null)

  def registerParser(parser: RecvPacketParser): Unit = {
    packetParsers(parser.packetId) = parser
  }

  def send(packet: SendPacket): Unit = {
    val data: ByteBuffer = ByteBuffer.allocate(0x800)

    data.clear()
    data.put(packet.id.toByte)
    packet.serialize(data)

    val encryptedData: Array[Byte] = new Array(data.position())
    crypto.encryptClient(data.array(), 0, encryptedData, 0, data.position())

    socket.getOutputStream.write(encryptedData, 0, data.position())

    if (logPackets) {
      logger.info(s"sent >>> $packet")
    }
  }

  var currentSize: Int = 0
  val compData: Array[Byte] = new Array[Byte](0x20000)
  val rawData: Array[Byte] = new Array[Byte](0x20000)

  def readPacket(): Option[RecvPacket] = {
    if (socket.isClosed) {
      None
    } else {
      var result: Option[RecvPacket] = None

      val in: InputStream = socket.getInputStream

      if (in.available() > 0) {
        val newRead: Int = in.read(compData, currentSize, compData.length - currentSize)
        crypto.decryptServer(compData, currentSize, compData, currentSize, newRead)
        currentSize += newRead
      }

      if (currentSize > 0) {
        val r = compression.decompress(compData, 0, currentSize, rawData, 0)
        (r.inSize, r.outSize, r.seenBoundary)

        if (r.inSize > currentSize) {
          throw new IllegalStateException(s"read ${r.inSize} bytes, while available buffer size was $currentSize")
        } else if (r.outSize <= 0) {
          logger.warn(s"failed to decompress packet data")
        } else {

          val packetId: Int = rawData(0) & 0xFF

          if (!r.seenBoundary) {
            logger.warn(f"didn't see compression boundary in packet $packetId%02x")
          } else {
            val handler = packetParsers(packetId)

            if (handler == null) {
              logger.info(f"unhandled packet: $packetId%02x with ${r.outSize} bytes")
            } else {
              val mis = new DataInputStream(new ByteArrayInputStream(rawData, 1, r.outSize - 1))
              try {
                val packet = handler.parse(mis, r.outSize)

                result = Some(packet)

                if (logPackets) {
                  logger.info(s"received <<< $packet")
                }
              } catch {
                case e: Exception =>
                  logger.error(f"failed to parse packet $packetId%02x", e)
              }
            }

            if (r.inSize < currentSize) {
              val newCurrentSize: Int = currentSize - r.inSize
              System.arraycopy(compData, r.inSize, compData, 0, newCurrentSize)
              currentSize = newCurrentSize
            } else {
              currentSize = 0
            }
          }
        }
      }

      result
    }
  }
}

object PacketManager {
  def withParsers(socket: Socket, newClient: Boolean): PacketManager = {
    val pm = new PacketManager(socket)

    pm.registerParser(ServerListPacketParser)
    pm.registerParser(ServerLoginPacketParser)
    pm.registerParser(SupportedFeaturesPacketParser)
    pm.registerParser(CharacterListPacketParser(newClient))
    pm.registerParser(ClientVersionPacketParser)
    pm.registerParser(LoginConfirmPacketParser)
    pm.registerParser(GeneralInfoPacketParser)
    pm.registerParser(SeasonInfoPacketParser)
    pm.registerParser(DrawPlayerPacketParser)
    pm.registerParser(OverallLightLevelPacketParser)
    pm.registerParser(PersonalLightLevelPacketParser)
    pm.registerParser(MobileIncomingPacketParser)
    pm.registerParser(MobileStatusPacketParser)
    pm.registerParser(WarModePacketParser)
    pm.registerParser(HealthColorPacketParser)
    pm.registerParser(ItemRevisionHashPacketParser)
    pm.registerParser(LoginCompletePacketParser)
    pm.registerParser(TimePacketParser)
    pm.registerParser(CompressedGumpPacketParser)
    pm.registerParser(MessageUnicodePacketParser)
    pm.registerParser(MessageAsciiPacketParser)
    pm.registerParser(ItemInfoPacketParser)
    pm.registerParser(DeletePacketParser)
    pm.registerParser(UpdatePlayerPacketParser)
    pm.registerParser(MobileAnimationPacketParser)
    pm.registerParser(SetWeatherPacketParser)
    pm.registerParser(MoveAckPacketParser)
    pm.registerParser(MoveRejPacketParser)
    pm.registerParser(LoginDeniedPacketParser)

    pm
  }
}
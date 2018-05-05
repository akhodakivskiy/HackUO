package kdkvsk.hackuo.network

import java.io._
import java.net.Socket
import java.nio.ByteBuffer

import com.typesafe.scalalogging.LazyLogging
import kdkvsk.hackuo.client.Cliloc
import kdkvsk.hackuo.client.compression.{Compression, NoCompression}
import kdkvsk.hackuo.client.crytpo.{Crypto, NoCrypto}
import kdkvsk.hackuo.network.packets.recv._
import kdkvsk.hackuo.network.packets.recvsend.{x72_WarModePacketParser, xBD_ClientVersionPacketParser, xBF_GeneralInfoPacketParser, xD6_ClilocResponsePacketParser}

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
                  logger.info(f"received <<< $packetId%02x $packet")
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
  def withParsers(socket: Socket, cliloc: Cliloc, newClient: Boolean): PacketManager = {
    val pm = new PacketManager(socket)

    pm.registerParser(x1B_LoginConfirmPacketParser)
    pm.registerParser(x1C_MessageAsciiPacketParser)
    pm.registerParser(x1D_DeletePacketParser)
    pm.registerParser(x2D_UpdateMobileStatusPacketParser)
    pm.registerParser(x2E_WearItemByMobilePacketParser)
    pm.registerParser(x3C_AddContainerItemsPacketParser)
    pm.registerParser(x4E_PersonalLightLevelPacketParser)
    pm.registerParser(x4F_OverallLightLevelPacketParser)
    pm.registerParser(x5B_TimePacketParser)
    pm.registerParser(x6D_PlayMidiMusicPacketParser)
    pm.registerParser(x6E_MobileAnimationPacketParser)
    pm.registerParser(x8C_ServerLoginPacketParser)
    pm.registerParser(x11_MobileStatusPacketParser)
    pm.registerParser(x17_HealthColorPacketParser)
    pm.registerParser(x20_DrawPlayerPacketParser)
    pm.registerParser(x21_MoveRejPacketParser)
    pm.registerParser(x22_MoveAckPacketParser)
    pm.registerParser(x23_DragAnimationPacketParser)
    pm.registerParser(x24_DrawContainerPacketParser)
    pm.registerParser(x25_AddItemToContainerPacketParser)
    pm.registerParser(x54_PlaySoundPacketParser)
    pm.registerParser(x55_LoginCompletePacketParser)
    pm.registerParser(x65_SetWeatherPacketParser)
    pm.registerParser(x72_WarModePacketParser)
    pm.registerParser(x76_ServerChangePacketParser)
    pm.registerParser(x77_UpdateMobilePacketParser)
    pm.registerParser(x78_MobileIncomingPacketParser)
    pm.registerParser(x82_LoginDeniedPacketParser)
    pm.registerParser(x88_OpenPaperdollPacketParser)
    pm.registerParser(xA1_HealthUpdatePacketParser)
    pm.registerParser(xA8_ServerListPacketParser)
    pm.registerParser(xA9_CharacterListPacketParser(newClient))
    pm.registerParser(xAE_MessageUnicodePacketParser)
    pm.registerParser(xB9_SupportedFeaturesPacketParser)
    pm.registerParser(xBC_SeasonInfoPacketParser)
    pm.registerParser(xBD_ClientVersionPacketParser)
    pm.registerParser(xBF_GeneralInfoPacketParser)
    pm.registerParser(xC0_GraphicalEffectPacketParser)
    pm.registerParser(xC1_ClilocMessagePacketParser(cliloc))
    pm.registerParser(xD6_ClilocResponsePacketParser(cliloc))
    pm.registerParser(xDC_ItemRevisionHashPacketParser)
    pm.registerParser(xDD_CompressedGumpPacketParser)
    pm.registerParser(xE2_NewMobileAnimationPacketParser)
    pm.registerParser(xF3_ItemInfoPacketParser)

    pm
  }
}
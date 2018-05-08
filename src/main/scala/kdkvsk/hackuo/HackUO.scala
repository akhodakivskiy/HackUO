package kdkvsk.hackuo

import java.net._
import java.nio.ByteBuffer
import java.nio.file.{Path, Paths}
import java.util.concurrent.ArrayBlockingQueue

import com.typesafe.scalalogging.LazyLogging
import kdkvsk.hackuo.client._
import kdkvsk.hackuo.client.compression.HuffmanCompression
import kdkvsk.hackuo.client.crytpo.{GameCrypto, LoginCrypto}
import kdkvsk.hackuo.model.common.{ClientFlag, ClientVersion}
import kdkvsk.hackuo.model.World
import kdkvsk.hackuo.network.PacketManager
import kdkvsk.hackuo.network.packets.recv._
import kdkvsk.hackuo.network.packets.send._
import kdkvsk.hackuo.model.composition.LoginData
import scopt.{OptionParser, Read}

import scala.concurrent.ExecutionContext
import scala.io.StdIn

object HackUO {

  case class Config(hostOpt: Option[InetAddress] = None,
                    portOpt: Option[Int] = None,
                    username: String = null,
                    password: String = null,
                    clientDir: Path = null,
                    clientVersion: ClientVersion = null,
                    clientLanguage: String = "enu",
                    serverName: String = null,
                    characterName: String = null,
                    clientFlagOpt: Option[ClientFlag.ValueSet] = None,
                    loginCount: Int = 0,
                    isEncrypted: Boolean = false) {
    val loginServerOpt: Option[(InetAddress, Int)] = {
      for {
        host <- hostOpt
        port <- portOpt
      } yield {
        (host, port)
      }
    }
  }

  import ClientVersion.clientVersionReader
  import ClientFlag.clientFlagReader

  implicit var pathReader: Read[Path] = Read.reads(s => Paths.get(s))

  val parser: OptionParser[Config] = new OptionParser[Config](this.getClass.getSimpleName) {
    opt[Path]("client-dir").required().text("path to the folder with UO client files").action((v, c) => c.copy(clientDir = v))
    opt[ClientVersion]("client-version").required().text("emulated client version").action((v, c) => c.copy(clientVersion = v))
    opt[String]("username").required().text("account username").action((v, c) => c.copy(username = v))
    opt[String]("password").required().text("account password").action((v, c) => c.copy(password = v))
    opt[String]("server").required().text("server name").action((v, c) => c.copy(serverName = v))
    opt[String]("character").required().text("character name").action((v, c) => c.copy(characterName = v))
    opt[String]("host").optional().text("server host name").action((v, c) => c.copy(hostOpt = Some(InetAddress.getByName(v))))
    opt[Int]("port").optional().text("server port").action((v, c) => c.copy(portOpt = Some(v)))
    opt[String]("client-language").optional().text("client language for cliloc").action((v, c) => c.copy(clientLanguage = v))
    opt[ClientFlag.ValueSet]("client-flag").optional().text(s"client flag indicating uo version").action((v, c) => c.copy(clientFlagOpt = Some(v)))
    opt[Int]("login-count").optional().text("login count (defaults to 0)").action((v, c) => c.copy(loginCount = v))
    opt[Unit]("encrypted").optional().text("OSI server encryption on").action((v, c) => c.copy(isEncrypted = true))
  }

  def main(args: Array[String]): Unit = {
    parser.parse(args, Config()).foreach { config =>
      val app = new HackUO(config)
      app.run()
    }
  }
}

class HackUO(config: HackUO.Config) extends LazyLogging {
  val client = new Client(config.clientDir, config.clientLanguage)

  val messageQueue: ArrayBlockingQueue[Message] = new ArrayBlockingQueue[Message](1000000)

  def run(): Unit = {
    val clientIp: InetAddress = InetAddress.getLocalHost

    val (serverName, serverIp, serverPort, authId) = login(clientIp)

    val socket: Socket = new Socket(serverIp, serverPort)

    val pm: PacketManager = PacketManager.withParsers(socket, client.cliloc, newClient = true)
    pm.setCompression(new HuffmanCompression())
    if (config.isEncrypted) {
      pm.setCrypto(new GameCrypto(authId))
    }

    val loginState: LoginData = LoginData(config.username, config.password,
      clientIp, serverIp, serverPort, authId, config.clientVersion, serverName, config.characterName,
      config.clientFlagOpt.getOrElse(ClientFlag.Latest), config.loginCount)

    val world: World = World.empty(client, loginState)

    startReadingPackets(pm)

    startProcessingPackets(pm, world)

    val authIdBuffer: ByteBuffer = ByteBuffer.allocate(4)
    authIdBuffer.putInt(authId)
    socket.getOutputStream.write(authIdBuffer.array(), 0, 4)

    messageQueue.put(StartupMessage)

    while (!socket.isClosed) {
      val command: String = StdIn.readLine("> ")
      messageQueue.put(REPLInput(command.split(" ").filter(_.nonEmpty).toList))
    }

    socket.close()
  }

  def handleResponse(response: Response, packetManager: PacketManager): Unit = {
    response match {
      case PacketResponse(packet) => packetManager.send(packet)
      case MultiResponse(responses) => responses.foreach(r => handleResponse(r, packetManager))
      case MessageResponse(message) => messageQueue.put(message)
      case LogResponse(text) => logger.info(text)
      case TerminateResponse(reason) =>
        logger.info(s"terminating due to: $reason")
        System.exit(0)
      case NoopResponse =>
      case EventResponse(e) =>
    }
  }

  def startProcessingPackets(packetManager: PacketManager, worldInit: World): Unit = {
    ExecutionContext.Implicits.global.execute(new Runnable {
      private var world: World = worldInit
      private var continue: Boolean = true

      def run(): Unit = {
        while (continue) {
          val message: Message = messageQueue.take()

          if (World.handler.isDefinedAt(message)) {
            val (newWorld, response) = World.handler(message).run(world).value

            handleResponse(response, packetManager)

            world = newWorld
          }

          if (message == ShutdownMessage) {
            continue = false
          }
        }
      }
    })
  }

  def startReadingPackets(packetManager: PacketManager): Unit = {
    ExecutionContext.Implicits.global.execute(new Runnable {
      def run(): Unit = {
        while (!packetManager.isClosed) {
          try {
            packetManager.readPacket().foreach { p =>
              messageQueue.put(PacketMessage(p))
            }
          } catch {
            case e: Exception => logger.error(s"error occurred", e)
          }

          Thread.sleep(10)
        }

        messageQueue.put(ShutdownMessage)
      }
    })
  }

  def login(clientIpAddress: InetAddress): (String, InetAddress, Int, Int) = {
    val (loginHost, loginPort) = config.loginServerOpt.orElse(ClientConfig.loginServerOpt(client.configLogin)).getOrElse {
      throw new IllegalArgumentException(s"please specify host and port as command line parameters, or in Login.cfg")
    }

    val nextLoginKey: Int = ClientConfig.nextLoginKey(client.configUo)
    val socket: Socket = new Socket(loginHost, loginPort)
    val pm: PacketManager = PacketManager.withParsers(socket, client.cliloc, newClient = true)

    val seedPacket = xEF_SeedPacket(clientIpAddress, config.clientVersion)
    pm.send(seedPacket)
    if (config.isEncrypted) {
      val (key1, key2) = config.clientVersion.keys
      pm.setCrypto(new LoginCrypto(xEF_SeedPacket.seed(clientIpAddress), key1, key2))
    }
    pm.send(x80_AccountLoginPacket(config.username, config.password, nextLoginKey))

    var repeat: Int = 100
    val delay: Long = 100L

    var serverName: String = null
    var ipAddress: InetAddress = null
    var port: Int = 0
    var authId: Int = 0

    var loggedIn: Boolean = false

    while (!loggedIn && repeat > 0) {
      pm.readPacket() match {
        case Some(xA8_ServerListPacket(infoFlag, servers)) =>
          servers.find(_.name == config.serverName) match {
            case Some(server) =>
              serverName = server.name
              pm.send(xA0_SelectServerPacket(server.index))
            case None =>
              logger.error(s"server '${config.serverName}' not found")
              System.exit(0)
          }
        case Some(x8C_ServerLoginPacket(ip, p, a)) =>
          ipAddress = ip
          port = p
          authId = a

          loggedIn = true
        case Some(x82_LoginDeniedPacket(reason)) =>
          logger.error(s"login denied due to $reason")
          System.exit(0)
        case Some(packet) =>
          logger.error(s"unexpected packet $packet")
          System.exit(0)
        case None =>
          repeat -= 1
          Thread.sleep(delay)
      }
    }

    socket.close()

    if (loggedIn) {
      (serverName, ipAddress, port, authId)
    } else {
      logger.error(s"login failed after ${delay * repeat} millis")
      System.exit(0)
      null
    }
  }
}


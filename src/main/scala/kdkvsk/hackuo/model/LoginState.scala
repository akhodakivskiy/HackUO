package kdkvsk.hackuo.model

import java.net.InetAddress

import kdkvsk.hackuo.model.common.ClientVersion
import kdkvsk.hackuo.network.packets.send.ClientFlag

case class LoginState(username: String,
                      password: String,
                      clientIp: InetAddress,
                      serverIp: InetAddress,
                      serverPort: Int,
                      authId: Int,
                      clientVersion: ClientVersion,
                      serverName: String,
                      characterName: String,
                      clientFlag: ClientFlag.Type,
                      loginCount: Int,
                      featuresFlag: Int = 0,
                      loggedIn: Boolean = false)


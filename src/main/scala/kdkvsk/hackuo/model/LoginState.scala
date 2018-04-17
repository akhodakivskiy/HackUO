package kdkvsk.hackuo.model

import java.net.InetAddress

import kdkvsk.hackuo.model.common.ClientVersion

case class LoginState(username: String,
                      password: String,
                      clientIp: InetAddress,
                      serverIp: InetAddress,
                      serverPort: Int,
                      authId: Int,
                      clientVersion: ClientVersion,
                      serverName: String,
                      characterName: String,
                      featuresFlag: Int = 0,
                      loggedIn: Boolean = false)


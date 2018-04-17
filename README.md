This is a stub of a headless (stealth) client for Ultima Online. The client is written in Scala, and a bit in Java without any external dependencies.

Currently implemented:

- Basic packet handling
- Walking
- OSI encryption

Not sure where this project is going tho...

Compiling:

```
$ sbt stage
```

Usage:

```
$ ./target/universal/stage/bin/hack-uo --help
  --host <value>           server host name
  --port <value>           server port
  --username <value>       account username
  --password <value>       account password
  --client-version <value>
                           emulated client version
  --next-login-key <value>
                           next login key (from uo.cfg)
  --server <value>         server name
  --character <value>      character name
  --client-flag <value>    client flag indicating uo version: (t2a, renaissance, third dawn, lbr, aos, se, sa, uo3d, reserved, 3d)
  --login-count <value>    login count (defaults to 0)
  --encrypted              OSI server encryption on
  ```

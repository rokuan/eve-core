package com.ideal.evecore.interpreter.remote

import java.io.IOException

import scala.util.{Failure, Success, Try}
import RemoteEndPointMessage._

/**
  * Created by Christophe on 11/03/2017.
  */
trait RemoteEndPoint extends StreamUtils {
  /**
    * Pings the client to ensure that the connection is still alive
    *
    * @return true if the connection is closed, false otherwise
    */
  def closeIfDown(): Boolean = Try {
    safe(writeCommand(Ping))
  } match {
    case Success(_) => false
    case Failure(_: IOException) =>
      try {
        socket.close()
      } catch {
        case _: Throwable =>
      }
      true
    case _ => false
  }

  /**
    * Sends a command to the client
    * @param cmd The command to be executed
    */
  protected def writeCommand(cmd: String): Unit
}

object RemoteEndPointMessage {
  val Ping = "PING"
}
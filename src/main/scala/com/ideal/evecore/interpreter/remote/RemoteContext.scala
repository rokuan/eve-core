package com.ideal.evecore.interpreter.remote

import java.io.IOException
import java.net.Socket

import com.ideal.evecore.interpreter.{Context, EveObject}

import scala.util.{Failure, Success, Try}


/**
 * Created by Christophe on 05/03/2017.
 */
class RemoteContext(val socket: Socket) extends Context with StreamUtils {
  import RemoteContextMessage._

  override def findItemsOfType(t: String): Option[EveObject] = Try {
    safe {
      writeCommand(FindItemsOfType)
      writeValue(t)
      readResultValue[EveObject]
    }
  } match {
    case Success(o) => o
    case Failure(_: Throwable) => Option.empty[EveObject]
  }

  /**
   * Sends a command to the client
   * @param cmd The command to be executed
   */
  protected def writeCommand(cmd: String) = {
    os.write(cmd.getBytes)
    os.flush()
  }

  /**
   * Pings the client to ensure that the connection is still alive
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
}

object RemoteContextMessage {
  val FindItemsOfType = "FTYP"
  val ObjectRequest = "ORQT"
  val Ping = "PING"
}

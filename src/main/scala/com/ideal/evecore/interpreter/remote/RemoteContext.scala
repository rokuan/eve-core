package com.ideal.evecore.interpreter.remote

import java.net.Socket

import com.ideal.evecore.common.Conversions._
import com.ideal.evecore.interpreter.{Context, EveObject}

import scala.util.{Failure, Success, Try}


/**
 * Created by Christophe on 05/03/2017.
 */
class RemoteContext(val socket: Socket) extends Context with RemoteEndPoint {
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
  override protected def writeCommand(cmd: String): Unit = {
    os.write(cmd.getBytes)
    os.flush()
  }
}

object RemoteContextMessage {
  val FindItemsOfType = "FTYP"
  val ObjectRequest = "ORQT"
}

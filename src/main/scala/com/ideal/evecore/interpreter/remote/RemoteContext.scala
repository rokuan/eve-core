package com.ideal.evecore.interpreter.remote

import java.net.Socket

import com.ideal.evecore.common.Conversions._
import com.ideal.evecore.interpreter.{EveStructuredObject, EveObjectList, Context, EveObject}

import scala.util.{Failure, Success, Try}


/**
 * Created by Christophe on 05/03/2017.
 */
class RemoteContext(val socket: Socket) extends Context with RemoteEndPoint {
  import RemoteContextMessage._

  override def findItemsOfType(t: String): Option[EveObjectList] = Try {
    safe {
      writeCommand(FindItemsOfType)
      writeValue(t)
      readResultValue[EveObjectList]
    }
  } match {
    case Success(o) => o
    case Failure(_: Throwable) => Option.empty[EveObjectList]
  }

  /**
   * Queries the context to find a single item of a certain type
   * @param t The type to query
   * @return A single object matching this type if some
   */
  override def findOneItemOfType(t: String): Option[EveStructuredObject] = Try {
    safe {
      writeCommand(FindOneItemOfType)
      writeValue(t)
      readResultValue[EveStructuredObject]
    }
  } match {
    case Success(o) => o
    case Failure(_: Throwable) => Option.empty[EveStructuredObject]
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
  val FindOneItemOfType = "FOTY"
  val ObjectRequest = "ORQT"
}

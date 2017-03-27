package com.ideal.evecore.interpreter.remote

import java.net.Socket

import com.ideal.evecore.common.Conversions._
import com.ideal.evecore.interpreter.{QueryContext, EveStructuredObject, EveObjectList, Context}
import com.ideal.evecore.io.command._

import scala.util.{Failure, Success, Try}


/**
 * Created by Christophe on 05/03/2017.
 */
class RemoteContext(protected val id: String, protected val socket: Socket) extends QueryContext with RemoteEndPoint {
  override def findItemsOfType(t: String): Option[EveObjectList] = Try {
    safe {
      writeCommand(FindItemsOfTypeCommand(t))
      readResultValue[EveObjectList]
    }
  } match {
    case Success(o) => o
    case Failure(_: Throwable) => Option.empty[EveObjectList]
  }

  /**
   * Queries the context to find a single item of a certain type
   *
   * @param t The type to query
   * @return A single object matching this type if some
   */
  override def findOneItemOfType(t: String): Option[EveStructuredObject] = Try {
    safe {
      writeCommand(FindOneItemOfTypeCommand(t))
      readResultValue[EveStructuredObject]
    }
  } match {
    case Success(o) => o
    case Failure(_: Throwable) => Option.empty[EveStructuredObject]
  }

  override def findById(id: String): Option[EveStructuredObject] = safe {
    writeCommand(FindItemByIdCommand(id))
    readResultValue[EveStructuredObject]
  }

  protected def writeCommand(command: ContextCommand) = {
    val userCommand = ContextRequestCommand(id, command)
    // TODO:
  }
}

object RemoteContextMessage {
  val ContextCommand = "CCMD"
  val FindItemsOfType = "FTYP"
  val FindOneItemOfType = "FOTY"
  val FindItemById = "FBID"
  val ObjectRequest = "ORQT"
}

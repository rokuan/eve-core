package com.ideal.evecore.interpreter.remote

import java.net.Socket

import com.ideal.evecore.common.Conversions._
import com.ideal.evecore.interpreter.{QueryContext, EveStructuredObject, EveObjectList, Context}

import scala.util.{Failure, Success, Try}


/**
 * Created by Christophe on 05/03/2017.
 */
class RemoteContext(protected val id: String, protected val socket: Socket) extends QueryContext with RemoteEndPoint {
  import RemoteContextMessage._

  override def findItemsOfType(t: String): Option[EveObjectList] = Try {
    safe {
      withHeader {
        writeCommand(FindItemsOfType)
        writeValue(t)
        readResultValue[EveObjectList]
      }
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
      withHeader {
        writeCommand(FindOneItemOfType)
        writeValue(t)
        readResultValue[EveStructuredObject]
      }
    }
  } match {
    case Success(o) => o
    case Failure(_: Throwable) => Option.empty[EveStructuredObject]
  }

  private final def withHeader[T](f: => T) = {
    writeCommand(ContextCommand)
    writeValue(id)
    f
  }

  override def findById(id: String): Option[EveStructuredObject] = safe {
    writeCommand(FindItemById)
    writeValue(id)
    readResultValue[EveStructuredObject]
  }
}

object RemoteContextMessage {
  val ContextCommand = "CCMD"
  val FindItemsOfType = "FTYP"
  val FindOneItemOfType = "FOTY"
  val FindItemById = "FBID"
  val ObjectRequest = "ORQT"
}

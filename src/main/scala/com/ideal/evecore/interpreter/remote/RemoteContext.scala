package com.ideal.evecore.interpreter.remote

import java.net.Socket

import com.ideal.evecore.common.Conversions._
import com.ideal.evecore.interpreter._
import com.ideal.evecore.io.{Serializers, SocketLockHandler}
import com.ideal.evecore.io.command._

import scala.util.{Failure, Success, Try}


/**
 * Created by Christophe on 05/03/2017.
 */
class RemoteContext(protected val id: String, protected val handler: SocketLockHandler) extends Context with QuerySource {
  implicit val formats = Serializers.buildRemoteFormats(id, handler)

  override def findItemsOfType(t: String): Option[EveObjectList] = Try {
    handler.resultProcess {
      writeCommand(FindItemsOfTypeCommand(t))
      handler.readResultResponse[EveObjectList]
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
    handler.resultProcess {
      writeCommand(FindOneItemOfTypeCommand(t))
      handler.readResultResponse[EveStructuredObject]
    }
  } match {
    case Success(o) => o
    case Failure(_: Throwable) => Option.empty[EveStructuredObject]
  }

  override def findById(id: String): Option[EveStructuredObject] = handler.resultProcess {
    writeCommand(FindItemByIdCommand(id))
    handler.readResultResponse[EveStructuredObject]
  }

  protected def writeCommand(command: ContextCommand) = {
    val userCommand = ContextRequestCommand(id, command)
    handler.writeUserCommand(userCommand)
  }
}

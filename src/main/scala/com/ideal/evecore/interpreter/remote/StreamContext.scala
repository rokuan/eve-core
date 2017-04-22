package com.ideal.evecore.interpreter.remote


import com.ideal.evecore.interpreter._
import com.ideal.evecore.io.{StreamHandler, Serializers}
import com.ideal.evecore.io.command.ObjectCommand
import com.ideal.evecore.io.command._

import scala.util.Try
import com.ideal.evecore.common.Conversions._

/**
  * Created by Christophe on 07/03/17.
  */
class StreamContext(private val contextId: String, protected val handler: StreamHandler, val context: Context) extends Context with ObjectStreamSource {
  implicit val formats = Serializers.buildRemoteFormats(handler, contextId)

  /**
    * Reads the commands that are sent from the server
    * @return
    */
  final def handleCommand(command: ContextCommand)(implicit requestId: Long) = Try {
    command match {
      case c: FindItemsOfTypeCommand => {
        val items = findItemsOfType(c.itemType)
        handler.writeResultResponse[EveObjectList](items)
      }
      case c: FindOneItemOfTypeCommand => {
        val result = findOneItemOfType(c.itemType)
        handler.writeResultResponse[EveStructuredObject](result)
      }
      case c: ObjectCommand => handleObjectCommand(c)
      case _ =>
    }
  }

  override def findItemsOfType(t: String): Option[EveObjectList] = context.findItemsOfType(t)

  /**
    * Queries the context to find a single item of a certain type
    * @param t The type to query
    * @return A single object matching this type if some
    */
  override def findOneItemOfType(t: String): Option[EveStructuredObject] = context.findOneItemOfType(t)

  override def findById(id: String): Option[EveStructuredObject] = context match {
    case q: QuerySource => q.findById(id)
    case _ => Option.empty[EveStructuredObject]
  }
}

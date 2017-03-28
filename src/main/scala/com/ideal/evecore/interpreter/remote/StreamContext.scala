package com.ideal.evecore.interpreter.remote

import java.net.Socket

import com.ideal.evecore.interpreter._
import com.ideal.evecore.io.Serializers.EveObjectSerializer
import com.ideal.evecore.io.command.ObjectCommand
import com.ideal.evecore.io.command._

import scala.util.Try
import com.ideal.evecore.common.Conversions._
import com.ideal.evecore.io.Readers.StringResultConverter

/**
 * Created by Christophe on 07/03/17.
 */
class StreamContext(private val contextId: String, protected val socket: Socket, val context: Context) extends QueryContext with StreamUtils {
  implicit val eveObjectSerializer = new EveObjectSerializer(contextId, socket)

  /**
   * Reads the commands that are sent from the server
   * @return
   */
  final def handleCommand(command: ContextCommand) = Try {
    safe {
      command match {
        case c: FindItemsOfTypeCommand => {
          val items = findItemsOfType(c.itemType)
          writeResultValue(items)
        }
        case c: FindOneItemOfTypeCommand => {
          val result = findOneItemOfType(c.itemType)
          writeResultValue(result)
        }
        case c: ObjectCommand => findById(c.objectId).map { o =>
          c.objectCommand match {
            case c: GetTypeCommand => writeValue(o.getType())
            case c: SetFieldCommand => o.set(c.field, c.value)
            case c: GetFieldCommand => writeResultValue(o.get(c.field))
            case c: SetStateCommand => o.setState(c.field, c.value)
            case c: GetStateCommand => writeResultValue(o.getState(c.field))
            case c: HasFieldCommand => writeValue(o.has(c.field))
            case c: HasStateCommand => writeValue(o.hasState(c.field))
            case _ =>
          }
        }
        case _ =>
      }
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
    case q: QueryContext => q.findById(id)
    case _ => Option.empty[EveStructuredObject]
  }
}

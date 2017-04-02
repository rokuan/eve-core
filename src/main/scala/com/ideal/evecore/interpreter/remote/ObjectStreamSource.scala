package com.ideal.evecore.interpreter.remote

import com.ideal.evecore.interpreter.{EveObject, QuerySource}
import com.ideal.evecore.io.Serializers.ResultSerializer
import com.ideal.evecore.io.command._
import com.ideal.evecore.common.Conversions._
import com.ideal.evecore.io.SocketLockHandler
import org.json4s.DefaultFormats

/**
 * Created by Christophe on 29/03/2017.
 */
trait ObjectStreamSource { this: QuerySource =>
  implicit val formats = DefaultFormats + new ResultSerializer[EveObject]()
  protected val handler: SocketLockHandler

  final def handleObjectCommand(command: ObjectCommand): Unit = {
    findById(command.objectId).map { o =>
      command.objectCommand match {
        case c: GetTypeCommand => handler.writeStringResponse(o.getType())
        case c: SetFieldCommand => o.set(c.field, c.value)
        case c: GetFieldCommand => handler.writeResultResponse(o.get(c.field))
        case c: SetStateCommand => o.setState(c.field, c.value)
        case c: GetStateCommand => handler.writeResultResponse(o.getState(c.field))
        case c: HasFieldCommand => handler.writeBooleanResponse(o.has(c.field))
        case c: HasStateCommand => handler.writeBooleanResponse(o.hasState(c.field))
        case _ =>
      }
    }
  }
}

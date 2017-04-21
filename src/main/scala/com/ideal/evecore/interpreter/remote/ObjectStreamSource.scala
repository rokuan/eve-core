package com.ideal.evecore.interpreter.remote

import com.ideal.evecore.interpreter.{EveObject, QuerySource}
import com.ideal.evecore.io.command._
import com.ideal.evecore.common.Conversions._
import com.ideal.evecore.io.StreamHandler
import org.json4s.Formats

/**
 * Created by Christophe on 29/03/2017.
 */
trait ObjectStreamSource extends QuerySource {
  protected val handler: StreamHandler

  final def handleObjectCommand(command: ObjectCommand)(implicit requestId: Long, formats: Formats): Unit = {
    findById(command.objectId).map { o =>
      command.objectCommand match {
        case c: GetTypeCommand => handler.writeStringResponse(o.getType())
        case c: SetFieldCommand => o.set(c.field, c.value)
        case c: GetFieldCommand => handler.writeResultResponse[EveObject](o.get(c.field))
        case c: SetStateCommand => o.setState(c.field, c.value)
        case c: GetStateCommand => handler.writeResultResponse[String](o.getState(c.field))
        case c: HasFieldCommand => handler.writeBooleanResponse(o.has(c.field))
        case c: HasStateCommand => handler.writeBooleanResponse(o.hasState(c.field))
        case _ =>
      }
    }
  }
}

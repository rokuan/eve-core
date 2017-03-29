package com.ideal.evecore.interpreter.remote

import com.ideal.evecore.interpreter.{EveObject, QuerySource}
import com.ideal.evecore.io.Serializers.ResultSerializer
import com.ideal.evecore.io.command._
import com.ideal.evecore.common.Conversions._
import org.json4s.DefaultFormats

/**
 * Created by Christophe on 29/03/2017.
 */
trait ObjectStreamSource { this: QuerySource with StreamUtils =>
  implicit val formats = DefaultFormats + new ResultSerializer[EveObject]()

  final def handleObjectCommand(command: ObjectCommand): Unit = {
    findById(command.objectId).map { o =>
      command.objectCommand match {
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
  }
}

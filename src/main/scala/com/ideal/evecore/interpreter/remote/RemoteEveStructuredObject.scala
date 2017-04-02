package com.ideal.evecore.interpreter.remote


import com.ideal.evecore.interpreter.{EveObject, EveStructuredObject}
import com.ideal.evecore.common.Conversions._
import com.ideal.evecore.io.{Serializers, SocketLockHandler}
import com.ideal.evecore.io.command._

/**
  * Created by Christophe on 05/03/2017.
  */
class RemoteEveStructuredObject(val domainId: String, val objectId: String, protected val handler: SocketLockHandler) extends EveStructuredObject {
  implicit val formats = Serializers.buildRemoteFormats(domainId, handler)

  override def getType(): String = handler.resultProcess {
    writeCommand(GetTypeCommand())
    handler.readStringResponse()
  }

  override def set(field: String, value: EveObject): Unit = writeCommand(SetFieldCommand(field, value))

  override def get(field: String): Option[EveObject] = handler.resultProcess {
    writeCommand(GetFieldCommand(field))
    handler.readResultResponse[EveObject]
  }

  override def getState(state: String): Option[String] = handler.resultProcess {
    writeCommand(GetStateCommand(state))
    handler.readResultResponse[String]
  }

  override def setState(state: String, value: String): Unit = writeCommand(SetStateCommand(state, value))

  override def has(field: String): Boolean = handler.resultProcess {
    writeCommand(HasFieldCommand(field))
    handler.readBooleanResponse()
  }

  override def hasState(state: String): Boolean = handler.resultProcess {
    writeCommand(HasStateCommand(state))
    handler.readBooleanResponse()
  }

  protected def writeCommand(command: EveStructuredObjectCommand) = {
    val userCommand = ObjectRequestCommand(domainId, objectId, command)
    handler.writeUserCommand(userCommand)
  }
}

object RemoteEveStructuredObjectMessage {
  val ObjectCommand = "OCMD"
  val GetType = "GTYP"
  val SetField = "SFLD"
  val GetField = "GFLD"
  val HasField = "HFLD"
  val GetState = "GSTE"
  val SetState = "SSTE"
  val HasState = "HSTE"
}

package com.ideal.evecore.interpreter.remote


import com.ideal.evecore.interpreter.{EveObject, EveStructuredObject}
import com.ideal.evecore.common.Conversions._
import com.ideal.evecore.io.message.Result
import com.ideal.evecore.io.{StreamHandler, Serializers, SocketLockHandler}
import com.ideal.evecore.io.command._

/**
  * Created by Christophe on 05/03/2017.
  */
class RemoteEveStructuredObject(val domainId: String, val objectId: String, protected val handler: StreamHandler) extends EveStructuredObject {
  implicit val formats = Serializers.buildRemoteFormats(handler, domainId)

  override def getType(): String = handler.stringOperation(GetTypeCommand())

  override def set(field: String, value: EveObject): Unit = handler.commandOperation(SetFieldCommand(field, value))

  override def get(field: String): Option[EveObject] = handler.resultOperation[Result[EveObject]](GetFieldCommand(field))

  override def getState(state: String): Option[String] = handler.resultOperation[Result[String]](GetStateCommand(state))

  override def setState(state: String, value: String): Unit = handler.commandOperation(SetStateCommand(state, value))

  override def has(field: String): Boolean = handler.booleanOperation(HasFieldCommand(field))

  override def hasState(state: String): Boolean = handler.booleanOperation(HasStateCommand(state))

  implicit protected def getCommand(command: EveStructuredObjectCommand): UserCommand = ObjectRequestCommand(domainId, objectId, command)
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

package com.ideal.evecore.interpreter.remote

import java.net.Socket

import com.ideal.evecore.interpreter.{EveObject, EveStructuredObject}
import com.ideal.evecore.io.Readers._
import com.ideal.evecore.common.Conversions._
import com.ideal.evecore.io.command._

/**
  * Created by Christophe on 05/03/2017.
  */
class RemoteEveStructuredObject(protected val contextId: String, private val objectId: String, protected val socket: Socket) extends EveStructuredObject with StreamUtils {
  override def getType(): String = safe {
    writeCommand(GetTypeCommand())
    readValue()
  }

  override def set(field: String, value: EveObject): Unit = safe {
    writeCommand(SetFieldCommand(field, value))
  }

  override def get(field: String): Option[EveObject] = safe {
    writeCommand(GetFieldCommand(field))
    readResultValue[EveObject]
  }

  override def getState(state: String): Option[String] = safe {
    writeCommand(GetStateCommand(state))
    readResultValue[String]
  }

  override def setState(state: String, value: String): Unit = safe {
    writeCommand(SetStateCommand(state, value))
  }

  /**
   * Sends a command to the corresponding remote EveStructuredObject
   * @param cmd
   */
  protected def writeCommand(command: EveStructuredObjectCommand) = {
    // TODO:
  }

  override def has(field: String): Boolean = safe {
    writeCommand(HasFieldCommand(field))
    readTest()
  }

  override def hasState(state: String): Boolean = safe {
    writeCommand(HasStateCommand(state))
    readTest()
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

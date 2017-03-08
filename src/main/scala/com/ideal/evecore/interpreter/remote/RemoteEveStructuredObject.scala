package com.ideal.evecore.interpreter.remote

import java.net.Socket

import com.ideal.evecore.interpreter.{EveObject, EveStructuredObject}
import com.ideal.evecore.io.Readers._

/**
  * Created by Christophe on 05/03/2017.
  */
class RemoteEveStructuredObject(private val objectId: String, protected val socket: Socket) extends EveStructuredObject with StreamUtils {
  import RemoteEveStructuredObjectMessage._

  override def getType(): String = safe {
    writeCommand(GetType)
    readValue()
  }

  override def set(field: String, value: EveObject): Unit = safe {
    writeCommand(SetField)
    writeValue(field)
    writeObject(value)
  }

  override def get(field: String): Option[EveObject] = safe {
    writeCommand(GetField)
    writeValue(field)
    readResultValue[EveObject]
  }

  override def getState(state: String): Option[String] = safe {
    writeCommand(GetState)
    writeValue(state)
    readResultValue[String]
  }

  override def setState(state: String, value: String): Unit = {
    writeCommand(SetState)
    writeValue(state)
    writeValue(value)
  }

  /**
   * Sends a command to the corresponding remote EveStructuredObject
   * @param cmd
   */
  protected def writeCommand(cmd: String) = {
    os.write(RemoteContextMessage.ObjectRequest.getBytes)
    os.flush()
    writeValue(objectId)
    os.write(cmd.getBytes)
    os.flush()
  }

  override def has(field: String): Boolean = {
    writeCommand(HasField)
    writeValue(field)
    readTest()
  }

  override def hasState(state: String): Boolean = {
    writeCommand(HasState)
    writeValue(state)
    readTest()
  }
}

object RemoteEveStructuredObjectMessage {
  val GetType = "GTYP"
  val SetField = "SFLD"
  val GetField = "GFLD"
  val HasField = "HFLD"
  val GetState = "GSTE"
  val SetState = "SSTE"
  val HasState = "HSTE"
}
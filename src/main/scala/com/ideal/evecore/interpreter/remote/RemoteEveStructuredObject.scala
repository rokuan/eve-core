package com.ideal.evecore.interpreter.remote

import java.net.Socket

import com.ideal.evecore.interpreter.{EveObject, EveStructuredObject}
import com.ideal.evecore.io.Readers._
import com.ideal.evecore.common.Conversions._

/**
  * Created by Christophe on 05/03/2017.
  */
class RemoteEveStructuredObject(protected val contextId: String, private val objectId: String, protected val socket: Socket) extends EveStructuredObject with StreamUtils {
  import RemoteEveStructuredObjectMessage._

  override def getType(): String = safeWithHeader {
    writeCommand(GetType)
    readValue()
  }

  override def set(field: String, value: EveObject): Unit = safeWithHeader {
    writeCommand(SetField)
    writeValue(field)
    writeObject(value)
  }

  override def get(field: String): Option[EveObject] = safeWithHeader {
    writeCommand(GetField)
    writeValue(field)
    readResultValue[EveObject]
  }

  override def getState(state: String): Option[String] = safeWithHeader {
    writeCommand(GetState)
    writeValue(state)
    readResultValue[String]
  }

  override def setState(state: String, value: String): Unit = safeWithHeader {
    writeCommand(SetState)
    writeValue(state)
    writeValue(value)
  }

  /**
   * Sends a command to the corresponding remote EveStructuredObject
   * @param cmd
   */
  override protected def writeCommand(cmd: String) = {
    os.write(RemoteContextMessage.ObjectRequest.getBytes)
    os.flush()
    writeValue(objectId)
    super.writeCommand(cmd)
  }

  override def has(field: String): Boolean = safeWithHeader {
    writeCommand(HasField)
    writeValue(field)
    readTest()
  }

  override def hasState(state: String): Boolean = safeWithHeader {
    writeCommand(HasState)
    writeValue(state)
    readTest()
  }

  private final def safeWithHeader[T](process: => T) = safe {
    writeCommand(ObjectCommand)
    writeValue(contextId)
    writeValue(objectId)
    process
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

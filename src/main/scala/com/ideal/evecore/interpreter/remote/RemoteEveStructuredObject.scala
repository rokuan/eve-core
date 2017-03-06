package com.ideal.evecore.interpreter.remote

import java.net.Socket

import com.ideal.evecore.interpreter.{EveObject, EveStructuredObject}
import com.ideal.evecore.io.message.Readers.EveObjectResultReader
import com.ideal.evecore.io.message.ResultReader
import com.ideal.evecore.io.message.Readers._
import org.json4s.JValue

/**
  * Created by Christophe on 05/03/2017.
  */
class RemoteEveStructuredObject(private val objectId: String, protected val socket: Socket) extends EveStructuredObject with ResultReader[EveObject] with StreamUtils {
  import RemoteEveStructuredObjectMessage._



  /*protected val is = socket.getInputStream
  protected val os = socket.getOutputStream*/

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

  protected def writeCommand(cmd: String) = {
    os.write(RemoteContextMessage.ObjectRequest.getBytes)
    os.flush()
    writeValue(objectId)
    os.write(cmd.getBytes)
    os.flush()
  }

  override def extract(o: JValue): EveObject = resultReader.extract(o)
}

object RemoteEveStructuredObjectMessage {
  val GetType = "GTYP"
  val SetField = "SFLD"
  val GetField = "GFLD"
  val GetState = "GSTE"
  val SetState = "SSTE"
}

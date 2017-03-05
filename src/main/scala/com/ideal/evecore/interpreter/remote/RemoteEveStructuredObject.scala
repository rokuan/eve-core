package com.ideal.evecore.interpreter.remote

import java.net.Socket

import com.ideal.evecore.interpreter.{EveObject, EveStructuredObject}
import com.ideal.evecore.io.message.Readers.EveObjectResultReader
import com.ideal.evecore.io.message.ResultReader
import com.ideal.evecore.io.message.Readers._

/**
  * Created by Christophe on 05/03/2017.
  */
class RemoteEveStructuredObject(private val objectId: String, protected val socket: Socket) extends EveStructuredObject with ResultReader[EveObject] {
  import RemoteEveStructuredObjectMessage._

  implicit val resultReader = new EveObjectResultReader(socket)
  //implicit val reader: Serializer[EveObject] = resultReader.reader

  protected val is = socket.getInputStream
  protected val os = socket.getOutputStream

  override def getType(): String = safe {
    writeCommand(GetType)
    readValue()
  }

  override def set(field: String, value: EveObject): Unit = safe {
    writeCommand(SetField)
    os.write(field.length)
    os.write(field.getBytes)
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

  protected def safe[T](process: T) = socket.synchronized(process)

  protected def writeCommand(cmd: String) = {
    val length = math.min(objectId.length, 256)
    os.write(length)
    os.write(objectId.getBytes(), 0, length)
    os.write(cmd.getBytes)
    os.flush()
  }

  protected def writeValue(s: String) = {
    os.write(s.length)
    os.write(s.getBytes)
    os.flush()
  }

  protected def writeObject(o: EveObject) = {}

  protected def readObject(): EveObject = { null /* TODO */ }

  protected def readValue(): String = {
    val source = scala.io.Source.fromInputStream(is)
    val data = new Array[Byte](4)

    if(is.read(data) != 1){
      var length = data.zipWithIndex.foldLeft(0){ case (acc, (size, index)) => acc + ((size & 0xFF) << (index * 8)) }
      val block = new Array[Byte](1024)
      val buffer = new StringBuilder()
      var read = 0

      while(length > 0){
        val read = is.read(block)

        if(read > 0) {
          buffer.append(new String(block, 0, read))
          length -= read
        } else {
          length = 0
        }
      }

      buffer.toString()
    } else {
      ""
    }
  }

  protected def readResultValue[T](implicit reader: ResultReader[T]): Option[T] = {
    val result = reader.readFrom(is)
    if(result.success){
      None
    } else {
      Some(result.value)
    }
  }
}

object RemoteEveStructuredObjectMessage {
  val GetType = "GTYP"
  val SetField = "SFLD"
  val GetField = "GFLD"
  val GetState = "GSTE"
  val SetState = "SSTE"
}

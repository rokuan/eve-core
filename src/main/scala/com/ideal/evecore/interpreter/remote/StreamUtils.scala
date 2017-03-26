package com.ideal.evecore.interpreter.remote

import com.ideal.evecore.interpreter.EveObject
import com.ideal.evecore.io.Readers.EveObjectResultConverter
import com.ideal.evecore.io.Streamers.{EveObjectStreamHandler, MessageStreamHandler}
import com.ideal.evecore.io.{StreamReader, StreamWriter}
import com.ideal.evecore.io.message.{Result, ResultReader, ResultWriter}
import org.json4s.jackson.JsonMethods
import org.json4s.native.Serialization._
import com.ideal.evecore.io.Readers._

/**
 * Created by Christophe on 06/03/17.
 */
trait StreamUtils extends BasicSocketUtils {
  implicit val resultConverter = new EveObjectResultConverter(socket)
  implicit val resultListConverter = new EveObjectListResultConverter(socket)
  implicit val resultStructuredObjectConverter = new EveStructuredObjectResultConverter(socket)
  implicit val resultStreamHandler = new EveObjectStreamHandler(socket)
  implicit val messageConverter = new MessageConverter(socket)
  implicit val messageHandler = new MessageStreamHandler(socket)

  protected def writeObject(o: EveObject) = {
    val json = write(o)
    writeValue(json)
  }

  protected def readObject(): EveObject = {
    val json = readValue()
    resultConverter.extract(JsonMethods.parse(json))
  }

  /**
    * Reads a command from the server
    * @return
    */
  protected def readCommand(): String = {
    val commandData = new Array[Byte](4)
    if(is.read(commandData) >= 0){
      new String(commandData)
    } else {
      null
    }
  }

  /**
    * Sends a command to the client
    * @param cmd The command to be executed
    */
  protected def writeCommand(cmd: String) = {
    os.write(cmd.getBytes)
    os.flush()
  }

  protected def readResultValue[T >: Null](implicit reader: ResultReader[T]): Result[T] = reader.readFrom(is)
  protected def writeResultValue[T >: Null](v: Result[T])(implicit writer: ResultWriter[T]): Unit = writer.writeTo(os, v)

  protected def readItem[T](implicit reader: StreamReader[T]): T = reader.readFrom(is)
  protected def writeItem[T](o: T)(implicit writer: StreamWriter[T]): Unit = writer.writeTo(os, o)

  protected final def safe[T](process: => T) = socket.synchronized(process)
}

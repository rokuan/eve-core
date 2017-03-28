package com.ideal.evecore.interpreter.remote

import com.ideal.evecore.interpreter.EveObject
import com.ideal.evecore.io.Readers.EveObjectResultConverter
import com.ideal.evecore.io.Streamers.{EveObjectStreamHandler, MessageStreamHandler}
import com.ideal.evecore.io.command.UserCommand
import com.ideal.evecore.io.{StreamReader, StreamWriter}
import com.ideal.evecore.io.message.{Result, ResultReader, ResultWriter}
import org.json4s.jackson.{Serialization, JsonMethods}
import org.json4s.native.Serialization._
import com.ideal.evecore.io.Readers._

/**
 * Created by Christophe on 06/03/17.
 */
trait StreamUtils extends BasicSocketUtils {
  protected def writeObject(o: EveObject) = {
    val json = write(o)
    writeValue(json)
  }

  protected def readObject(): EveObject = {
    val json = readValue()
    resultConverter.extract(JsonMethods.parse(json))
  }

  protected def readUserCommand(): UserCommand = {
    val json = readValue()
    Serialization.read[UserCommand](json)
  }

  protected def writeUserCommand(command: UserCommand) = {
    val json = Serialization.write[UserCommand](command)
    writeValue(json)
  }

  /**
    * Reads a command from the server
    * @return
    */

  protected def readResultValue[T >: Null](implicit reader: ResultReader[T]): Result[T] = reader.readFrom(is)
  protected def writeResultValue[T >: Null](v: Result[T])(implicit writer: ResultWriter[T]): Unit = writer.writeTo(os, v)

  protected def readItem[T](implicit reader: StreamReader[T]): T = reader.readFrom(is)
  protected def writeItem[T](o: T)(implicit writer: StreamWriter[T]): Unit = writer.writeTo(os, o)

  protected final def safe[T](process: => T) = socket.synchronized(process)
}

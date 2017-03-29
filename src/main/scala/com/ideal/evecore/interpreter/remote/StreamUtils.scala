package com.ideal.evecore.interpreter.remote

import com.ideal.evecore.interpreter.EveObject
import com.ideal.evecore.io.command.UserCommand
import com.ideal.evecore.io.message.Result
import org.json4s.Formats
import org.json4s.jackson.{Serialization}

/**
 * Created by Christophe on 06/03/17.
 */
trait StreamUtils extends BasicSocketUtils {
  protected def readObject()(implicit formats: Formats): EveObject = readItem[EveObject]
  protected def writeObject(o: EveObject)(implicit formats: Formats) = writeItem[EveObject](o)

  protected def readUserCommand()(implicit formats: Formats): UserCommand = readItem[UserCommand]
  protected def writeUserCommand(command: UserCommand)(implicit formats: Formats) = writeItem[UserCommand](command)

  /**
    * Reads a command from the server
    * @return
    */

  protected def readResultValue[T >: Null](implicit formats: Formats, m: Manifest[T]): Result[T] = readItem[Result[T]]
  protected def writeResultValue[T >: Null](r: Result[T])(implicit formats: Formats): Unit = writeItem[Result[T]](r)

  protected def readItem[T <: AnyRef](implicit formats: Formats, m: Manifest[T]): T = {
    val json = readValue()
    Serialization.read[T](json)
  }
  protected def writeItem[T <: AnyRef](o: T)(implicit formats: Formats): Unit = {
    val json = Serialization.write[T](o)
    writeValue(json)
  }

  protected final def safe[T](process: => T) = socket.synchronized(process)
}

package com.ideal.evecore.io

import java.net.Socket
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import java.util.concurrent.locks.ReentrantLock

import com.ideal.evecore.interpreter.remote.BasicSocketUtils
import ExecutionToken._
import com.ideal.evecore.interpreter.EveObject
import com.ideal.evecore.io.command.UserCommand
import com.ideal.evecore.io.message.Result
import org.json4s.Formats
import org.json4s.jackson.Serialization

/**
  * Created by Christophe on 02/04/2017.
  */
class SocketLockHandler(val socket: Socket) extends BasicSocketUtils with Runnable {
  protected val lock = new ReentrantLock()
  private val commandCondition = lock.newCondition()
  private val resultCondition = lock.newCondition()
  private val commandAvailable = new AtomicInteger(UndefinedToken)
  private val running = new AtomicBoolean(true)

  override def run(): Unit = {
    while(running.get()){
      try {
        val token = is.read()
        commandAvailable.set(token)
        if (token == SocketHandler.CommandToken) {
          commandCondition.signal()
        } else if (token == SocketHandler.ResultToken) {
          resultCondition.signal()
        } else {
          running.set(false)
        }
      } catch {
        case _: Throwable => running.set(false)
      }
    }
  }

  def commandProcess[T](process: UserCommand => T)(implicit formats: Formats) = {
    while (commandAvailable.get() != CommandToken) {
      commandCondition.await()
    }
    commandAvailable.set(UndefinedToken)
    lock.lock()
    try {
      process(readCommandResponse())
    } finally {
      lock.unlock()
    }
  }

  def resultProcess[T](process: => T) = {
    lock.lock()
    try {
      process
    } finally {
      lock.unlock()
    }
  }

  private def waitForResult(): Unit = {
    while (commandAvailable.get() != ResultToken) {
      resultCondition.await()
    }
    commandAvailable.set(UndefinedToken)
  }

  def readResultResponse[T >: Null <: AnyRef](implicit formats: Formats, m: Manifest[T]): Result[T] = {
    waitForResult()
    readItem[Result[T]]
  }

  def readObjectResponse[T <: AnyRef](implicit formats: Formats, m: Manifest[T]): T = {
    waitForResult()
    readItem[T]
  }

  def readStringResponse(): String = {
    waitForResult()
    readValue()
  }

  def readBooleanResponse(): Boolean = {
    waitForResult()
    readTest()
  }

  private def readCommandResponse()(implicit formats: Formats): UserCommand = {
    readItem[UserCommand]
  }

  def writeObject(o: EveObject)(implicit formats: Formats) = writeResponseItem[EveObject](o)

  def writeUserCommand(command: UserCommand)(implicit formats: Formats) = writeCommandItem(command)

  def writeResultResponse[T >: Null](r: Result[T])(implicit formats: Formats): Unit = writeResponseItem[Result[T]](r)

  def writeObjectResponse[T <: AnyRef](o: T)(implicit formats: Formats) = writeResponseItem[T](o)

  def writeStringResponse(s: String): Unit = {
    os.write(ResultToken)
    writeValue(s)
  }

  def writeBooleanResponse(b: Boolean): Unit = {
    os.write(ResultToken)
    writeValue(b)
  }

  private final def writeCommandItem(o: UserCommand)(implicit formats: Formats): Unit = {
    os.write(CommandToken)
    writeItem[UserCommand](o)
  }

  private final def writeResponseItem[T <: AnyRef](o: T)(implicit formats: Formats): Unit = {
    os.write(ResultToken)
    writeItem[T](o)
  }

  private def readItem[T <: AnyRef](implicit formats: Formats, m: Manifest[T]): T = {
    val json = readValue()
    Serialization.read[T](json)
  }

  private def writeItem[T <: AnyRef](o: T)(implicit formats: Formats): Unit = {
    val json = Serialization.write[T](o)
    writeValue(json)
  }
}

object ExecutionToken {
  val UndefinedToken = 0
  val CommandToken = 1
  val ResultToken = 2
}

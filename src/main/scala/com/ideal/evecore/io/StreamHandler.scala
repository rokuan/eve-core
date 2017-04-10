package com.ideal.evecore.io

import java.net.Socket
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.{AtomicLong, AtomicBoolean}

import com.ideal.evecore.interpreter.remote.StreamUtils
import StreamHandler._
import com.ideal.evecore.io.command.UserCommand
import com.ideal.evecore.util.{PendingAtomicBoolean, PendingAtomicReference}
import org.json4s.Formats
import org.json4s.jackson.Serialization

class StreamHandler(val socket: Socket) extends StreamUtils with Runnable {
  implicit val formats = Serializers.buildBasicFormats()

  protected val running = new AtomicBoolean(true)
  protected val objectResults = collection.mutable.Map[Long, PendingAtomicReference[String]]()
  protected val stringResults = collection.mutable.Map[Long, PendingAtomicReference[String]]()
  protected val booleanResults = collection.mutable.Map[Long, PendingAtomicBoolean]()
  protected val stamp = new AtomicLong(0)
  protected val commands = new LinkedBlockingQueue[(Long, String)](1)

  override def run(): Unit = {
    while (running.get()) {
      is.synchronized {
        val token = is.read()

        token match {
          case CommandToken => handleUserCommand()
          case BooleanResult => handleBooleanAnswer()
          case StringResult => handleStringAnswer()
          case ObjectResult => handleObjectAnswer()
          case _ =>
        }
      }
    }
  }

  protected def handleBooleanAnswer() = {
    val operationId = readValue().toLong
    val test = readTest()
    booleanResults.remove(operationId).map { reference =>
      reference.set(test)
      reference.synchronized(reference.notify())
    }
  }

  protected def handleStringAnswer() = {
    val operationId = readValue().toLong
    val value = readValue()
    stringResults.remove(operationId).map { reference =>
      reference.set(value)
      reference.synchronized(reference.notify)
    }
  }

  protected def handleObjectAnswer() = {
    val operationId = readValue().toLong
    val json = readValue()
    objectResults.remove(operationId).map { reference =>
      reference.set(json)
      reference.synchronized(reference.notify)
    }
  }

  def handleUserCommand(): Unit = {
    val requestId = readValue().toLong
    val command = readValue()
    commands.offer(requestId, command)
  }

  def booleanOperation(command: UserCommand)(implicit formats: Formats): Boolean = {
    val operationId = stamp.incrementAndGet()
    val value = new PendingAtomicBoolean()
    os.synchronized {
      booleanResults.put(operationId, value)
      os.write(CommandToken)
      writeValue(operationId.toString)
      writeUserCommand(command)
    }
    value.get()
  }

  def stringOperation(command: UserCommand)(implicit formats: Formats): String = {
    val operationId = stamp.incrementAndGet()
    val value = new PendingAtomicReference[String]()
    os.synchronized {
      stringResults.put(operationId, value)
      os.write(CommandToken)
      writeValue(operationId.toString)
      writeUserCommand(command)
    }
    value.get()
  }

  def resultOperation[T <: AnyRef](command: UserCommand)(implicit formats: Formats, m: Manifest[T]): T = {
    val operationId = stamp.incrementAndGet()
    val currentThread = Thread.currentThread()
    val value = new PendingAtomicReference[String]()
    os.synchronized {
      objectResults.put(operationId, value)
      os.write(CommandToken)
      writeValue(operationId.toString)
      writeUserCommand(command)
    }
    Serialization.read[T](value.get())
  }

  def commandOperation(command: UserCommand)(implicit formats: Formats): Unit = {
    val requestId = stamp.incrementAndGet()
    os.synchronized {
      os.write(CommandToken)
      writeValue(requestId.toString)
      writeUserCommand(command)
    }
  }

  def nextCommand()(implicit formats: Formats): (Long, UserCommand) = {
    val pair = commands.take()
    (pair._1, Serialization.read[UserCommand](pair._2))
  }

  def writeResponse[T <: AnyRef](response: T)(implicit requestId: Long, formats: Formats) = os.synchronized {
    os.write(ObjectResult)
    writeValue(requestId.toString)
    writeItem[T](response)
  }

  def writeStringResponse(response: String)(implicit requestId: Long) = os.synchronized {
    os.write(StringResult)
    writeValue(requestId.toString)
    writeValue(response)
  }

  def writeBooleanResponse(response: Boolean)(implicit requestId: Long) = os.synchronized {
    os.write(BooleanResult)
    writeValue(requestId.toString)
    writeValue(response)
  }
}

object StreamHandler {
  val CommandToken = 0
  val BooleanResult = 1
  val StringResult = 2
  val ObjectResult = 3
}

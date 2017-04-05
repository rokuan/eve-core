package com.ideal.evecore.io

import java.net.Socket
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.{AtomicLong, AtomicBoolean}

import com.ideal.evecore.interpreter.remote.StreamUtils
import StreamHandler._
import com.ideal.evecore.io.command.UserCommand
import org.json4s.Formats

class StreamHandler(val socket: Socket) extends StreamUtils with Runnable {
  implicit val formats = Serializers.buildBasicFormats()

  protected val running = new AtomicBoolean(true)
  protected val pendingOperations = collection.mutable.Map[Long, Thread]()
  protected val stamp = new AtomicLong(0)
  protected val commands = new LinkedBlockingQueue[(Long, UserCommand)](1)

  override def run(): Unit = {
    while (running.get()) {
      is.synchronized {
        val token = is.read()

        token match {
          case CommandToken => handleUserCommand()
          case AnswerToken => handleAnswer()
          case _ =>
        }
      }
    }
  }

  def resultOperation[T <: AnyRef](command: UserCommand)(implicit formats: Formats, m: Manifest[T]): T = {
    val operationId = stamp.incrementAndGet()
    val currentThread = Thread.currentThread()
    os.synchronized {
      os.write(CommandToken)
      pendingOperations.put(operationId, currentThread)
      writeValue(operationId.toString)
      writeUserCommand(command)
      currentThread.synchronized(currentThread.wait())
      readItem[T]
    }
  }

  def stringOperation(command: UserCommand)(implicit formats: Formats): String = {
    val operationId = stamp.incrementAndGet()
    val currentThread = Thread.currentThread()
    os.synchronized {
      os.write(CommandToken)
      pendingOperations.put(operationId, currentThread)
      writeValue(operationId.toString)
      writeUserCommand(command)
      currentThread.synchronized(currentThread.wait())
      readValue()
    }
  }

  def booleanOperation(command: UserCommand)(implicit formats: Formats): Boolean = {
    val operationId = stamp.incrementAndGet()
    val currentThread = Thread.currentThread()
    os.synchronized {
      os.write(CommandToken)
      pendingOperations.put(operationId, currentThread)
      writeValue(operationId.toString)
      writeUserCommand(command)
      Thread.currentThread().wait
      readTest()
    }
  }

  def commandOperation(command: UserCommand)(implicit formats: Formats): Unit = {
    val requestId = stamp.incrementAndGet()
    os.synchronized {
      os.write(CommandToken)
      writeValue(requestId.toString)
      writeUserCommand(command)
    }
  }

  def nextCommand()(implicit formats: Formats): (Long, UserCommand) = commands.take()

  def handleUserCommand(): Unit = {
    val requestId = readValue().toLong
    val command = readUserCommand()
    commands.offer(requestId, command)
  }

  def handleAnswer(): Unit = {
    val answerId = readValue()
    pendingOperations.remove(answerId.toLong).foreach(t => t.synchronized(t.notify()))
  }

  def writeResponse[T <: AnyRef](response: T)(implicit requestId: Long, formats: Formats) = os.synchronized {
    os.write(AnswerToken)
    writeValue(requestId.toString)
    writeItem[T](response)
  }

  def writeStringResponse(response: String)(implicit requestId: Long) = os.synchronized {
    os.write(AnswerToken)
    writeValue(requestId.toString)
    writeValue(response)
  }

  def writeBooleanResponse(response: Boolean)(implicit requestId: Long) = os.synchronized {
    os.write(AnswerToken)
    writeValue(requestId.toString)
    writeValue(response)
  }
}

object StreamHandler {
  val CommandToken = 0
  val AnswerToken = 1
}

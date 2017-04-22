package com.ideal.evecore.io

import java.net.Socket
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.{AtomicLong, AtomicBoolean}

import com.ideal.evecore.interpreter.remote.StreamUtils
import StreamHandler._
import com.ideal.evecore.io.command.UserCommand
import com.ideal.evecore.io.message.Result
import com.ideal.evecore.util.{PendingAtomicBoolean, PendingAtomicReference}
import org.json4s.{Extraction, Formats}
import org.json4s.JsonAST.{JString, JObject}
import org.json4s.jackson.{JsonMethods, Serialization}
import org.json4s.JsonDSL._

class StreamHandler(val socket: Socket) extends StreamUtils with Runnable {
  implicit val formats = Serializers.buildRemoteFormats(this)

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
          case -1 => running.set(false)
          case _ =>
        }
      }
    }
  }

  protected def handleBooleanAnswer() = {
    val operationId = readValue().toLong
    val test = readTest()
    booleanResults.remove(operationId).foreach(_.set(test))
  }

  protected def handleStringAnswer() = {
    val operationId = readValue().toLong
    val value = readValue()
    stringResults.remove(operationId).foreach(_.set(value))
  }

  protected def handleObjectAnswer() = {
    val operationId = readValue().toLong
    val json = readValue()
    objectResults.remove(operationId).foreach(_.set(json))
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

  def objectOperation[T <: AnyRef](command: UserCommand)(implicit formats: Formats, m: Manifest[T]): T  = {
    val operationId = stamp.incrementAndGet()
    val currentThread = Thread.currentThread()
    val value = new PendingAtomicReference[String]()
    os.synchronized {
      objectResults.put(operationId, value)
      os.write(CommandToken)
      writeValue(operationId.toString)
      writeUserCommand(command)
    }
    val json = value.get()
    Serialization.read[T](json)
  }

  def resultOperation[T >: Null](command: UserCommand)(implicit formats: Formats, m: Manifest[T]): Result[T] = {
    val operationId = stamp.incrementAndGet()
    val currentThread = Thread.currentThread()
    val value = new PendingAtomicReference[String]()
    os.synchronized {
      objectResults.put(operationId, value)
      os.write(CommandToken)
      writeValue(operationId.toString)
      writeUserCommand(command)
    }
    val json = value.get()
    val o = JsonMethods.parse(json).asInstanceOf[JObject]

    if ((o \ "success").extractOpt[Boolean].getOrElse(false)) {
      Result.Ok((o \ "value").extract[T])
    } else {
      Result.Ko((o \ "error").extract[String])
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

  def writeResultResponse[T >: Null](response: Result[T])(implicit requestId: Long) = os.synchronized {
    os.write(ObjectResult)
    writeValue(requestId.toString)
    val o =
      if (response.success) {
        ("success" -> true) ~ ("value" -> Extraction.decompose(response.value))
      } else {
        ("success" -> false) ~ ("error" -> JString(response.error))
      }
    writeValue(JsonMethods.compact(o))
  }
}

object StreamHandler {
  val CommandToken = 0
  val BooleanResult = 1
  val StringResult = 2
  val ObjectResult = 3
}

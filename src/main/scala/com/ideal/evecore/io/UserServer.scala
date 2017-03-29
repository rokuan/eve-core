package com.ideal.evecore.io

import java.net.{ServerSocket, Socket}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

import com.ideal.evecore.interpreter.{Environment, Evaluator}
import com.ideal.evecore.interpreter.remote.{BasicSocketUtils, RemoteContext, RemoteReceiver, StreamUtils}
import com.ideal.evecore.io.command.{EvaluateCommand, RegisterReceiverCommand, RegisterContextCommand, UserCommand}
import com.ideal.evecore.universe.World
import com.ideal.evecore.users.Session
import com.rokuan.calliopecore.parser.AbstractParser
import org.json4s.DefaultFormats
import org.json4s.jackson.Serialization

import scala.util.Try
import scala.util.control.Breaks
import com.ideal.evecore.common.Conversions._

/**
  * Created by Christophe on 26/03/2017.
  */
abstract class UserServer[T <: Session](val port: Int) extends Thread with AutoCloseable {
  protected val server = new ServerSocket(port)
  private val running = new AtomicBoolean(true)

  override def run(): Unit = {
    val breaks = new Breaks

    while(running.get()){
      val client = server.accept()
      val wrapper = new BasicSocketUtils {
        override protected val socket: Socket = client
      }
      val login = wrapper.readValue()
      val password = wrapper.readValue()

      authenticate(login, password).map { s =>
        wrapper.writeValue(true)
        val t = connectUser(client, s)
        t.start()
      }.getOrElse {
        wrapper.writeValue(false)
        client.close()
      }
    }
  }

  def authenticate(login: String, password: String): Try[T]
  def connectUser(socket: Socket, user: T): UserSocket[T]

  override def close(): Unit = {
    try { server.close() } catch { case _: Throwable => }
  }
}

abstract class UserSocket[T <: Session](protected val socket: Socket, protected val session: T) extends Thread with StreamUtils {
  implicit val formats = DefaultFormats + UserCommand.UserCommandSerializer

  protected val environment: Environment
  protected val world: World
  protected val evaluator: Evaluator
  protected val parser: AbstractParser

  private val running = new AtomicBoolean(true)

  private val receivers = collection.mutable.Map[String, RemoteReceiver]()
  private val contexts = collection.mutable.Map[String, RemoteContext]()

  private final val idGenerator = new AtomicInteger(0)

  def getSession(): T = session

  override final def run(): Unit = {
    while(running.get()){
      Try(readCommand()).map { cmd =>
        cmd match {
          case _: RegisterContextCommand => registerContext()
          case _: RegisterReceiverCommand => registerReceiver()
          case ec: EvaluateCommand => evaluate(ec.text)
          case null => running.set(false)
          case _ =>
        }
      }.getOrElse(running.set(false))
    }
  }

  private def evaluate(text: String) = Try {
    safe {
      val obj = parser.parseText(text)
      val result = evaluator.eval(obj)
      writeResultValue(result)
    }
  }

  private def registerReceiver() = safe {
    val receiverId = freshId()
    val remoteReceiver = new RemoteReceiver(receiverId, socket)
    receivers.put(receiverId, remoteReceiver)
    world.registerReceiver(remoteReceiver)
    writeValue(receiverId)
  }

  private def unregisterReceiver() = safe {
    val receiverId = readValue()
    receivers.get(receiverId).map(world.unregisterReceiver)
  }

  private def registerContext() = safe {
    val contextId = freshId()
    val remoteContext = new RemoteContext(contextId, socket)
    contexts.put(contextId, remoteContext)
    environment.addContext(remoteContext)
    writeValue(contextId)
  }

  private def unregisterContext() = safe {
    val contextId = readValue()
    contexts.get(contextId).map(environment.removeContext)
  }

  protected def readCommand(): UserCommand = {
    val json = readValue()
    Serialization.read[UserCommand](json)
  }

  private final def freshId(): String = idGenerator.incrementAndGet().toString
}

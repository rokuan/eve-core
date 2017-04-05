package com.ideal.evecore.io

import java.net.{ServerSocket, Socket}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

import com.ideal.evecore.interpreter.{EveObject, Environment, Evaluator}
import com.ideal.evecore.interpreter.remote._
import com.ideal.evecore.io.command._
import com.ideal.evecore.io.message.Result
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
      val wrapper = new SocketWrapper(client)
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

abstract class UserSocket[T <: Session](protected val socket: Socket, protected val session: T) extends Thread /*with StreamUtils*/ {
  implicit val formats = DefaultFormats + UserCommand.UserCommandSerializer

  protected val environment: Environment
  protected val world: World
  protected val evaluator: Evaluator
  protected val parser: AbstractParser

  private val running = new AtomicBoolean(true)

  private val receivers = collection.mutable.Map[String, RemoteReceiver]()
  private val contexts = collection.mutable.Map[String, RemoteContext]()

  //private val handler = new SocketHandler(socket)
  //private val handler = new SocketLockHandler(socket)
  private val handler = new StreamHandler(socket)
  // TODO: proper syntax
  new Thread(handler).start()

  private final val idGenerator = new AtomicInteger(0)

  def getSession(): T = session

  override final def run(): Unit = {
    while(running.get()){
      Try {
        val request = handler.nextCommand()
        implicit val requestId = request._1
        request._2 match {
          case _: RegisterContextCommand => registerContext()
          case _: RegisterReceiverCommand => registerReceiver()
          case urc: UnregisterReceiverCommand => unregisterReceiver(urc.receiverId)
          case ucc: UnregisterContextCommand => unregisterContext(ucc.contextId)
          case ec: EvaluateCommand => evaluate(ec.text)
          case null => running.set(false)
          case _ =>
        }
      }.getOrElse(running.set(false))
    }
  }

  private def evaluate(text: String)(implicit requestId: Long) = {
    val obj = parser.parseText(text)
    val result = evaluator.eval(obj)
    // TODO: in a big Try
    handler.writeResponse[Result[EveObject]](result)
  }

  private def registerReceiver()(implicit requestId: Long) = {
    val receiverId = freshId()
    val remoteReceiver = new RemoteReceiver(receiverId, handler)
    receivers.put(receiverId, remoteReceiver)
    world.registerReceiver(remoteReceiver)
    handler.writeStringResponse(receiverId)
  }

  private def unregisterReceiver(receiverId: String) = receivers.get(receiverId).map(world.unregisterReceiver)

  private def registerContext()(implicit requestId: Long) = {
    val contextId = freshId()
    val remoteContext = new RemoteContext(contextId, handler)
    contexts.put(contextId, remoteContext)
    environment.addContext(remoteContext)
    handler.writeStringResponse(contextId)
  }

  private def unregisterContext(contextId: String) = contexts.get(contextId).map(environment.removeContext)

  private final def freshId(): String = idGenerator.incrementAndGet().toString
}

package com.ideal.evecore.io

import java.net.{ServerSocket, Socket}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

import com.ideal.evecore.interpreter.{QuerySource, EveObject, Environment, Evaluator}
import com.ideal.evecore.interpreter.remote._
import com.ideal.evecore.io.command._
import com.ideal.evecore.io.message.Result
import com.ideal.evecore.universe.World
import com.ideal.evecore.users.Session
import com.rokuan.calliopecore.parser.AbstractParser
import org.json4s.DefaultFormats

import scala.util.{Success, Failure, Try}
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
  protected val environment: Environment
  protected val world: World
  protected val evaluator: Evaluator
  protected val parser: AbstractParser

  private val running = new AtomicBoolean(true)

  private val receivers = collection.mutable.Map[String, RemoteReceiver]()
  private val contexts = collection.mutable.Map[String, RemoteContext]()
  private val sources = collection.mutable.Map[String, QuerySource]()

  private val handler = new StreamHandler(socket)
  // TODO: proper syntax
  new Thread(handler).start()

  private final val idGenerator = new AtomicInteger(0)

  implicit val formats = Serializers.buildRemoteFormats(handler)

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
          case orc: ObjectRequestCommand => redirectObjectCommand(orc)
          case ec: EvaluateCommand => evaluate(ec.text)
          case null => running.set(false)
          case _ =>
        }
      }.getOrElse(running.set(false))
    }
  }

  private def evaluate(text: String)(implicit requestId: Long) = {
    Try {
      val obj = parser.parseText(text)
      val result = evaluator.eval(obj)
      // TODO: in a big Try
      result
    } match {
      case Success(r) => handler.writeResultResponse[EveObject](r)
      case Failure(e) => handler.writeResultResponse[EveObject](Result.Ko(e))
    }
  }

  private def redirectObjectCommand(orc: ObjectRequestCommand)(implicit requestId: Long) = {
    orc.objectCommand match {
      case GetTypeCommand(_) =>
        val result = handler.stringOperation(orc)
        handler.writeStringResponse(result)
      case GetFieldCommand(field, _) =>
        val result = handler.resultOperation[EveObject](orc)
        handler.writeResultResponse(result)
      case HasFieldCommand(field, _) =>
        val result = handler.booleanOperation(orc)
        handler.writeBooleanResponse(result)
      case GetStateCommand(field, _) =>
        val result = handler.stringOperation(orc)
        handler.writeStringResponse(result)
      case HasStateCommand(field, _) =>
        val result = handler.booleanOperation(orc)
        handler.writeBooleanResponse(result)
      case _ => handler.commandOperation(orc)
    }
  }

  private def registerReceiver()(implicit requestId: Long) = {
    val receiverId = freshId()
    val remoteReceiver = new RemoteReceiver(receiverId, handler)
    receivers.put(receiverId, remoteReceiver)
    sources.put(receiverId, remoteReceiver)
    world.registerReceiver(remoteReceiver)
    handler.writeStringResponse(receiverId)
  }

  private def unregisterReceiver(receiverId: String) = receivers.remove(receiverId).foreach(world.unregisterReceiver)

  private def registerContext()(implicit requestId: Long) = {
    val contextId = freshId()
    val remoteContext = new RemoteContext(contextId, handler)
    contexts.put(contextId, remoteContext)
    sources.put(contextId, remoteContext)
    environment.addContext(remoteContext)
    handler.writeStringResponse(contextId)
  }

  private def unregisterContext(contextId: String) = contexts.remove(contextId).foreach(environment.removeContext)

  private final def freshId(): String = idGenerator.incrementAndGet().toString
}

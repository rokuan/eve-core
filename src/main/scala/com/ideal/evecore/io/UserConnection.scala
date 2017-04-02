package com.ideal.evecore.io

import java.net.Socket
import java.util.concurrent.atomic.AtomicBoolean

import com.ideal.evecore.interpreter.{EveObject, Context}
import com.ideal.evecore.interpreter.remote.{ObjectStreamSource, StreamReceiver, StreamContext, StreamUtils}
import com.ideal.evecore.io.command._
import com.ideal.evecore.universe.receiver.Receiver
import org.json4s.DefaultFormats


/**
  * Created by Christophe on 26/03/2017.
  */
class UserConnection(val host: String, val port: Int, val credentials: Credentials) extends Thread with StreamUtils {
  implicit val formats = DefaultFormats + UserCommand.UserCommandSerializer

  protected val socket = new Socket(host, port)
  protected val contexts = collection.mutable.Map[String, StreamContext]()
  protected val receivers = collection.mutable.Map[String, StreamReceiver]()
  protected val sources = collection.mutable.Map[String, ObjectStreamSource]()

  protected val handler = new SocketLockHandler(socket)
  // TODO: better syntax
  new Thread(handler).start()

  private val running = new AtomicBoolean(true)
  private val logged = new AtomicBoolean(true)

  authenticate()

  def registerReceiver(r: Receiver) = handler.resultProcess {
    handler.writeUserCommand(RegisterReceiverCommand())
    val receiverId = handler.readStringResponse()
    val streamReceiver = new StreamReceiver(receiverId, handler, r)
    receivers.put(receiverId, streamReceiver)
    sources.put(receiverId, streamReceiver)
  }

  def registerContext(c: Context) = handler.resultProcess {
    handler.writeUserCommand(RegisterContextCommand())
    val contextId = handler.readStringResponse()
    val streamContext = new StreamContext(contextId, handler, c)
    contexts.put(contextId, streamContext)
    sources.put(contextId, streamContext)
  }

  def evaluate(text: String) = handler.resultProcess {
    handler.writeUserCommand(EvaluateCommand(text))
    handler.readObjectResponse[EveObject]
  }

  private def authenticate(): Unit = {
    writeValue(credentials.login)
    writeValue(credentials.password)

    if(!readTest()){
      disconnect()
      throw new Exception("Failed to authenticate")
    }
  }

  override def run(): Unit = {
    while (running.get()) {
      handler.commandProcess { command =>
        command match {
          case rrc: ReceiverRequestCommand => executeReceiverCommand(rrc)
          case crc: ContextRequestCommand => executeContextCommand(crc)
          case orc: ObjectRequestCommand => executeObjectCommand(orc)
          case null => running.set(false)
          case _ =>
        }
      }
    }
  }

  def disconnect() = {
    running.set(false)
    socket.close()
  }

  protected def executeReceiverCommand(rrc: ReceiverRequestCommand) = receivers.get(rrc.receiverId).map(_.handleCommand(rrc.receiverCommand))

  protected def executeContextCommand(crc: ContextRequestCommand) = contexts.get(crc.contextId).map(_.handleCommand(crc.contextCommand))

  protected def executeObjectCommand(orc: ObjectRequestCommand) = {
    val delegateCommand = ObjectCommand(orc.objectId, orc.objectCommand)
    sources.get(orc.domainId).map(_.handleObjectCommand(delegateCommand))
  }
}

class Credentials(val login: String, val password: String)

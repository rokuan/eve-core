package com.ideal.evecore.io

import java.net.Socket
import java.util.concurrent.atomic.AtomicBoolean

import com.ideal.evecore.interpreter.QueryContext
import com.ideal.evecore.interpreter.remote.StreamUtils
import com.ideal.evecore.io.command._
import com.ideal.evecore.universe.receiver.Receiver
import org.json4s.jackson.Serialization


/**
 * Created by Christophe on 26/03/2017.
 */
class UserConnection(val host: String, val port: Int) extends Thread with StreamUtils {
  protected val socket = new Socket(host, port)
  protected val contexts = collection.mutable.Map[String, QueryContext]()
  protected val receivers = collection.mutable.Map[String, Receiver]()
  private val running = new AtomicBoolean(true)

  def registerReceiver(r: Receiver) = safe {
    writeCommand(RegisterReceiverCommand())
    val receiverId = readValue()
    receivers.put(receiverId, r)
  }

  def registerContext(c: QueryContext) = safe {
    writeCommand(RegisterContextCommand())
    val contextId = readValue()
    contexts.put(contextId, c)
  }

  def disconnect() = {
    running.set(false)
    socket.close()
  }

  override def run(): Unit = {
    while(running.get()){
      val command = readCommand()
      command match {
        case rrc: ReceiverRequestCommand => executeReceiverCommand(rrc)
        case crc: ContextRequestCommand => executeContextCommand(crc)
        case orc: ObjectRequestCommand => executeObjectCommand(orc)
        case null => running.set(false)
        case _ =>
      }
    }
  }

  protected def executeReceiverCommand(rrc: ReceiverRequestCommand) = {
    receivers.get(rrc.receiverId).map { r =>
      rrc.receiverCommand match {
        // TODO:
        case _ =>
      }
    }
  }

  protected def executeContextCommand(crc: ContextRequestCommand) = {
    contexts.get(crc.contextId).map { r =>
      crc.contextCommand match {
        // TODO:
        case _ =>
      }
    }
  }

  protected def executeObjectCommand(orc: ObjectRequestCommand) = {
    contexts.get(orc.contextId).flatMap(_.findById(orc.objectId)).map { o =>
      orc.objectCommand match {
        // TODO:
        case _ =>
      }
    }
  }

  protected def readCommand(): UserCommand = {
    val json = readValue()
    Serialization.read[UserCommand](json)
  }

  protected def writeCommand(command: UserCommand) = {
    val json = Serialization.write[UserCommand](command)
    writeValue(json)
  }
}

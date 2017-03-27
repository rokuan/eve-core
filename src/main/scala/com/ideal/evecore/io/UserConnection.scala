package com.ideal.evecore.io

import java.net.Socket
import java.util.concurrent.atomic.AtomicBoolean

import com.ideal.evecore.interpreter.QueryContext
import com.ideal.evecore.interpreter.remote.{StreamContext, StreamReceiver, StreamUtils}
import com.ideal.evecore.universe.receiver.Receiver


/**
  * Created by Christophe on 26/03/2017.
  */
class UserConnection(val host: String, val port: Int) extends Thread with StreamUtils {
  import SocketMessage._
  import com.ideal.evecore.interpreter.remote.RemoteEveStructuredObjectMessage._
  protected val socket = new Socket(host, port)
  protected val contexts = collection.mutable.Map[String, StreamContext]()
  protected val receivers = collection.mutable.Map[String, StreamReceiver]()
  private val running = new AtomicBoolean(true)

  def registerReceiver(r: Receiver) = safe {
    writeCommand(RegisterReceiver)
    val receiverId = readValue()
    val streamReceiver = new StreamReceiver(receiverId, socket, r)
    receivers.put(receiverId, streamReceiver)
  }

  def registerContext(c: QueryContext) = safe {
    writeCommand(RegisterContext)
    val contextId = readValue()
    val streamContext = new StreamContext(contextId, socket, c)
    contexts.put(contextId, streamContext)
  }

  def disconnect() = {
    running.set(false)
    socket.close()
  }

  override def run(): Unit = {
    while(running.get()){
      val command = readCommand()
      command match {
        case ObjectCommand => executeObjectCommand()
        case null => running.set(false)
      }
    }
  }

  protected def executeObjectCommand() = {
    val contextId = readValue()
    contexts.get(contextId).map(_.handleCommand())
  }
}

package com.ideal.evecore.io

import java.net.Socket

import com.ideal.evecore.interpreter.QueryContext
import com.ideal.evecore.interpreter.remote.{StreamContext, StreamReceiver, StreamUtils}
import com.ideal.evecore.universe.receiver.Receiver

/**
  * Created by Christophe on 26/03/2017.
  */
class UserConnection(val host: String, val port: Int) extends StreamUtils {
  import SocketMessage._
  protected val socket = new Socket(host, port)
  protected val contexts = collection.mutable.Map[String, StreamContext]()
  protected val receivers = collection.mutable.Map[String, StreamReceiver]()

  def registerReceiver(r: Receiver) = {
    writeCommand(RegisterReceiver)
    val receiverId = readValue()
    val streamReceiver = new StreamReceiver(receiverId, socket, r)
    receivers.put(receiverId, streamReceiver)
  }

  def registerContext(c: QueryContext) = {
    writeCommand(RegisterContext)
    val contextId = readValue()
    val streamContext = new StreamContext(contextId, socket, c)
    contexts.put(contextId, streamContext)
  }
}

package com.ideal.evecore.interpreter.remote

import java.net.Socket

import com.ideal.evecore.common.Mapping.Mapping
import com.ideal.evecore.interpreter.EveObject
import com.ideal.evecore.universe.ValueMatcher
import com.ideal.evecore.universe.receiver.{Message, Receiver}
import RemoteReceiverMessage._
import com.ideal.evecore.common.Conversions._
import com.ideal.evecore.io.Streamers._

import scala.util.Try

/**
  * Created by Christophe on 09/03/17.
  */
class RemoteReceiver(protected val id: String, protected val socket: Socket) extends Receiver with RemoteEndPoint {
  /**
    * Called to initialize this receiver
    */
  override def initReceiver(): Unit = safeWithHeader(writeCommand(InitReceiver))

  /**
    * Retrieves this receiver's name
    *
    * @return This receiver's name
    */
  override def getReceiverName(): String = safeWithHeader {
    writeCommand(GetReceiverName)
    readValue()
  }

  /**
    * Returns the mapping defining the types of messages this receiver can handle
    *
    * @return A mapping containing the definition field of this receiver
    */
  override def getMappings(): Mapping[ValueMatcher] = safeWithHeader {
    writeCommand(GetMappings)
    readItem[Mapping[ValueMatcher]]
  }

  /**
    * Executes the message
    *
    * @param message The message to process
    * @return The result of the operation
    */
  override def handleMessage(message: Message): Try[EveObject] = safeWithHeader {
    writeCommand(HandleMessage)
    writeItem(message)
    readResultValue[EveObject]
  }

  /**
    * Called when destroying this receiver, to make sure everything is cleaned up
    */
  override def destroyReceiver(): Unit = safeWithHeader(writeCommand(DestroyReceiver))

  private final def safeWithHeader[T](f: => T) = safe {
    writeCommand(ReceiverCommand)
    writeValue(id)
    f
  }
}

object RemoteReceiverMessage {
  val ReceiverCommand = "RCMD"
  val InitReceiver = "IRCV"
  val DestroyReceiver = "DRCV"
  val HandleMessage = "HMSG"
  val GetMappings = "GMAP"
  val GetReceiverName = "GRNM"
}

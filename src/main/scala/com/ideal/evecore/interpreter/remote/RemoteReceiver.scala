package com.ideal.evecore.interpreter.remote

import java.net.Socket

import com.ideal.evecore.common.Mapping.Mapping
import com.ideal.evecore.interpreter.EveObject
import com.ideal.evecore.io.command.ReceiverCommand
import com.ideal.evecore.io.command._
import com.ideal.evecore.universe.ValueMatcher
import com.ideal.evecore.universe.receiver.{EveObjectMessage, Message, Receiver}
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
  override def initReceiver(): Unit = safe(writeCommand(InitReceiverCommand()))

  /**
    * Retrieves this receiver's name
    *
    * @return This receiver's name
    */
  override def getReceiverName(): String = safe {
    writeCommand(GetReceiverNameCommand())
    readValue()
  }

  /**
    * Returns the mapping defining the types of messages this receiver can handle
    *
    * @return A mapping containing the definition field of this receiver
    */
  override def getMappings(): Mapping[ValueMatcher] = safe {
    writeCommand(GetMappingsCommand())
    readItem[Mapping[ValueMatcher]]
  }

  /**
    * Executes the message
    *
    * @param message The message to process
    * @return The result of the operation
    */
  override def handleMessage(message: EveObjectMessage): Try[EveObject] = safe {
    writeCommand(HandleMessageCommand(message))
    readResultValue[EveObject]
  }

  /**
    * Called when destroying this receiver, to make sure everything is cleaned up
    */
  override def destroyReceiver(): Unit = safe { writeCommand(DestroyReceiverCommand()) }
  
  protected def writeCommand(command: ReceiverCommand) = {
    val userCommand = ReceiverRequestCommand(id, command)
    // TODO:
  }
}

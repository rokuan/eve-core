package com.ideal.evecore.interpreter.remote

import java.net.Socket

import com.ideal.evecore.common.Mapping.Mapping
import com.ideal.evecore.interpreter.{EveStructuredObject, QuerySource, EveObject}
import com.ideal.evecore.io.Serializers
import com.ideal.evecore.io.command.ReceiverCommand
import com.ideal.evecore.io.command._
import com.ideal.evecore.universe.ValueMatcher
import com.ideal.evecore.universe.receiver.{EveObjectMessage, Receiver}
import com.ideal.evecore.common.Conversions._

import scala.util.Try

/**
  * Created by Christophe on 09/03/17.
  */
class RemoteReceiver(protected val id: String, protected val socket: Socket) extends Receiver with QuerySource with StreamUtils {
  implicit val formats = Serializers.buildRemoteFormats(id, socket)

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
  
  override def findById(id: String): Option[EveStructuredObject] = safe {
    writeCommand(FindItemByIdCommand(id))
    readResultValue[EveStructuredObject]
  }

  protected def writeCommand(command: ReceiverCommand) = {
    val userCommand = ReceiverRequestCommand(id, command)
    writeUserCommand(userCommand)
  }
}

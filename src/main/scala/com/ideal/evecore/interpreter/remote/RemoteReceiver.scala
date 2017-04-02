package com.ideal.evecore.interpreter.remote


import com.ideal.evecore.common.Mapping.Mapping
import com.ideal.evecore.interpreter.{EveObject, EveStructuredObject, QuerySource}
import com.ideal.evecore.io.{Serializers, SocketLockHandler}
import com.ideal.evecore.io.command.ReceiverCommand
import com.ideal.evecore.io.command._
import com.ideal.evecore.universe.ValueMatcher
import com.ideal.evecore.universe.receiver.{EveObjectMessage, Receiver}
import com.ideal.evecore.common.Conversions._

import scala.util.Try

/**
  * Created by Christophe on 09/03/17.
  */
class RemoteReceiver(protected val id: String, protected val handler: SocketLockHandler) extends Receiver with QuerySource {
  implicit val formats = Serializers.buildRemoteFormats(id, handler)

  /**
    * Called to initialize this receiver
    */
  override def initReceiver(): Unit = writeCommand(InitReceiverCommand())

  /**
    * Retrieves this receiver's name
    *
    * @return This receiver's name
    */
  override def getReceiverName(): String = handler.resultProcess {
    writeCommand(GetReceiverNameCommand())
    handler.readStringResponse()
  }

  /**
    * Returns the mapping defining the types of messages this receiver can handle
    *
    * @return A mapping containing the definition field of this receiver
    */
  override def getMappings(): Mapping[ValueMatcher] = handler.resultProcess {
    writeCommand(GetMappingsCommand())
    handler.readObjectResponse[Mapping[ValueMatcher]]
  }

  /**
    * Executes the message
    *
    * @param message The message to process
    * @return The result of the operation
    */
  override def handleMessage(message: EveObjectMessage): Try[EveObject] = handler.resultProcess {
    writeCommand(HandleMessageCommand(message))
    handler.readResultResponse[EveObject]
  }

  /**
    * Called when destroying this receiver, to make sure everything is cleaned up
    */
  override def destroyReceiver(): Unit = writeCommand(DestroyReceiverCommand())
  
  override def findById(id: String): Option[EveStructuredObject] = handler.resultProcess {
    writeCommand(FindItemByIdCommand(id))
    handler.readResultResponse[EveStructuredObject]
  }

  protected def writeCommand(command: ReceiverCommand) = {
    val userCommand = ReceiverRequestCommand(id, command)
    handler.writeUserCommand(userCommand)
  }
}

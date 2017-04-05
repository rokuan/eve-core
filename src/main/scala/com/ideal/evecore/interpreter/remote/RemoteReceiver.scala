package com.ideal.evecore.interpreter.remote


import com.ideal.evecore.common.Mapping.Mapping
import com.ideal.evecore.interpreter.{EveObject, EveStructuredObject, QuerySource}
import com.ideal.evecore.io.message.Result
import com.ideal.evecore.io.{StreamHandler, Serializers, SocketLockHandler}
import com.ideal.evecore.io.command.ReceiverCommand
import com.ideal.evecore.io.command._
import com.ideal.evecore.universe.ValueMatcher
import com.ideal.evecore.universe.receiver.{EveObjectMessage, Receiver}
import com.ideal.evecore.common.Conversions._

import scala.util.Try

/**
  * Created by Christophe on 09/03/17.
  */
class RemoteReceiver(protected val id: String, protected val handler: StreamHandler) extends Receiver with QuerySource {
  implicit val formats = Serializers.buildRemoteFormats(id, handler)

  /**
    * Called to initialize this receiver
    */
  override def initReceiver(): Unit = handler.commandOperation(InitReceiverCommand())

  /**
    * Retrieves this receiver's name
    *
    * @return This receiver's name
    */
  override def getReceiverName(): String = handler.stringOperation(GetReceiverNameCommand())

  /**
    * Returns the mapping defining the types of messages this receiver can handle
    *
    * @return A mapping containing the definition field of this receiver
    */
  override def getMappings(): Mapping[ValueMatcher] = handler.resultOperation[Mapping[ValueMatcher]](GetMappingsCommand())

  /**
    * Executes the message
    *
    * @param message The message to process
    * @return The result of the operation
    */
  override def handleMessage(message: EveObjectMessage): Try[EveObject] = handler.resultOperation[Result[EveObject]](HandleMessageCommand(message))

  /**
    * Called when destroying this receiver, to make sure everything is cleaned up
    */
  override def destroyReceiver(): Unit = handler.commandOperation(DestroyReceiverCommand())
  
  override def findById(id: String): Option[EveStructuredObject] = handler.resultOperation[Result[EveStructuredObject]](FindItemByIdCommand(id))

  implicit protected def getCommand(command: ReceiverCommand): UserCommand = ReceiverRequestCommand(id, command)
}

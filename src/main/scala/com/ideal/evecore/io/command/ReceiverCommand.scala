package com.ideal.evecore.io.command

import com.ideal.evecore.interpreter.remote.RemoteReceiverMessage._
import com.ideal.evecore.universe.receiver.Message

/**
 * Created by Christophe on 27/03/2017.
 */
trait ReceiverCommand

case class InitReceiverCommand(command: String = InitReceiver) extends ReceiverCommand
case class DestroyReceiverCommand(command: String = DestroyReceiver) extends ReceiverCommand
case class HandleMessageCommand(message: Message, command: String = HandleMessage) extends ReceiverCommand
case class GetReceiverNameCommand(command: String = GetReceiverName) extends ReceiverCommand
case class GetMappingsCommand(command: String = GetMappings) extends ReceiverCommand


package com.ideal.evecore.io.command

import com.ideal.evecore.io.SocketMessage._

/**
 * Created by Christophe on 27/03/2017.
 */
trait UserCommand

case class RegisterReceiverCommand(command: String = RegisterReceiver) extends UserCommand
case class RegisterContextCommand(command: String = RegisterContext) extends UserCommand
case class ReceiverRequestCommand(receiverId: String, receiverCommand: ReceiverCommand, command: String = CallReceiverMethod) extends UserCommand
case class ContextRequestCommand(contextId: String, contextCommand: ContextCommand, command: String = CallContextMethod) extends UserCommand
case class ObjectRequestCommand(contextId: String, objectId: String, objectCommand: EveStructuredObjectCommand, command: String = CallObjectMethod) extends UserCommand

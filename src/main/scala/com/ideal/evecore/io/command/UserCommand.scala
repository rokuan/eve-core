package com.ideal.evecore.io.command

import org.json4s.{DefaultFormats, Extraction, CustomSerializer}
import org.json4s.JsonAST.JObject
import UserCommand._

/**
 * Created by Christophe on 27/03/2017.
 */
trait UserCommand

case class RegisterReceiverCommand(command: String = RegisterReceiver) extends UserCommand
case class RegisterContextCommand(command: String = RegisterContext) extends UserCommand
case class ReceiverRequestCommand(receiverId: String, receiverCommand: ReceiverCommand, command: String = CallReceiverMethod) extends UserCommand
case class ContextRequestCommand(contextId: String, contextCommand: ContextCommand, command: String = CallContextMethod) extends UserCommand
case class ObjectRequestCommand(contextId: String, objectId: String, objectCommand: EveStructuredObjectCommand, command: String = CallObjectMethod) extends UserCommand
case class PingCommand(command: String = Ping) extends UserCommand

object UserCommand {
  import com.ideal.evecore.io.Serializers.Formats

  val RegisterReceiver = "RRCV"
  val RegisterContext = "RCTX"
  val CallReceiverMethod = "CRMT"
  val CallContextMethod = "CCMT"
  val CallObjectMethod = "COMT"
  val Ping = "PING"

  implicit val UserCommandSerializer = new CustomSerializer[UserCommand](data => ({
    case o: JObject => (o \ "command").extract[String] match {
      case RegisterReceiver => RegisterReceiverCommand()
      case RegisterContext => RegisterContextCommand()
      case CallReceiverMethod => o.extract[ReceiverRequestCommand]
      case CallContextMethod => o.extract[ContextRequestCommand]
      case CallObjectMethod => o.extract[ObjectRequestCommand]
      case Ping => PingCommand()
    }
  }, {
    case rrc: RegisterReceiverCommand => Extraction.decompose(rrc)
    case rcc: RegisterContextCommand => Extraction.decompose(rcc)
    case rrc: ReceiverRequestCommand => Extraction.decompose(rrc)
    case crc: ContextRequestCommand => Extraction.decompose(crc)
    case orc: ObjectRequestCommand => Extraction.decompose(orc)
    case pc: PingCommand => Extraction.decompose(pc)
  }))
}
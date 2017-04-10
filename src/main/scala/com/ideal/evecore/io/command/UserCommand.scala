package com.ideal.evecore.io.command

import org.json4s.{Extraction, CustomSerializer}
import org.json4s.JsonAST.JObject
import UserCommand._

/**
 * Created by Christophe on 27/03/2017.
 */
trait UserCommand

case class RegisterReceiverCommand(command: String = RegisterReceiver) extends UserCommand
case class UnregisterReceiverCommand(receiverId: String, command: String = UnregisterReceiver) extends UserCommand
case class RegisterContextCommand(command: String = RegisterContext) extends UserCommand
case class UnregisterContextCommand(contextId: String, command: String = UnregisterContext) extends UserCommand
case class ReceiverRequestCommand(receiverId: String, receiverCommand: ReceiverCommand, command: String = CallReceiverMethod) extends UserCommand
case class ContextRequestCommand(contextId: String, contextCommand: ContextCommand, command: String = CallContextMethod) extends UserCommand
case class ObjectRequestCommand(domainId: String, objectId: String, objectCommand: EveStructuredObjectCommand, command: String = CallObjectMethod) extends UserCommand
case class EvaluateCommand(text: String, command: String = Evaluate) extends UserCommand
case class PingCommand(command: String = Ping) extends UserCommand

object UserCommand {
  import com.ideal.evecore.io.Serializers.Formats

  val Evaluate = "EVAL"
  val RegisterReceiver = "RRCV"
  val UnregisterReceiver = "URCV"
  val RegisterContext = "RCTX"
  val UnregisterContext = "UCTX"
  val CallReceiverMethod = "CRMT"
  val CallContextMethod = "CCMT"
  val CallObjectMethod = "COMT"
  val Ping = "PING"

  implicit val UserCommandSerializer = new CustomSerializer[UserCommand](data => ({
    case o: JObject => (o \ "command").extract[String] match {
      case Evaluate => o.extract[EvaluateCommand]
      case RegisterReceiver => RegisterReceiverCommand()
      case UnregisterReceiver => o.extract[UnregisterReceiverCommand]
      case RegisterContext => RegisterContextCommand()
      case UnregisterContext => o.extract[UnregisterContextCommand]
      case CallReceiverMethod => o.extract[ReceiverRequestCommand]
      case CallContextMethod => o.extract[ContextRequestCommand]
      case CallObjectMethod => o.extract[ObjectRequestCommand]
      case Ping => PingCommand()
    }
  }, {
    case ec: EvaluateCommand => Extraction.decompose(ec)
    case rrc: RegisterReceiverCommand => Extraction.decompose(rrc)
    case urc: UnregisterReceiverCommand => Extraction.decompose(urc)
    case rcc: RegisterContextCommand => Extraction.decompose(rcc)
    case ucc: UnregisterContextCommand => Extraction.decompose(ucc)
    case rrc: ReceiverRequestCommand => Extraction.decompose(rrc)
    case crc: ContextRequestCommand => Extraction.decompose(crc)
    case orc: ObjectRequestCommand => Extraction.decompose(orc)
    case pc: PingCommand => Extraction.decompose(pc)
  }))
}
package com.ideal.evecore.io.command

import ReceiverCommand._
import com.ideal.evecore.universe.receiver.Message
import org.json4s.{DefaultFormats, Extraction, CustomSerializer}
import org.json4s.JsonAST.JObject

/**
 * Created by Christophe on 27/03/2017.
 */
trait ReceiverCommand

case class InitReceiverCommand(command: String = InitReceiver) extends ReceiverCommand
case class DestroyReceiverCommand(command: String = DestroyReceiver) extends ReceiverCommand
case class HandleMessageCommand(message: Message, command: String = HandleMessage) extends ReceiverCommand
case class GetReceiverNameCommand(command: String = GetReceiverName) extends ReceiverCommand
case class GetMappingsCommand(command: String = GetMappings) extends ReceiverCommand

object ReceiverCommand {
  import com.ideal.evecore.io.Serializers.Formats

  val ReceiverCommand = "RCMD"
  val InitReceiver = "IRCV"
  val DestroyReceiver = "DRCV"
  val HandleMessage = "HMSG"
  val GetMappings = "GMAP"
  val GetReceiverName = "GRNM"

  implicit val ReceiverCommandSerializer = new CustomSerializer[ReceiverCommand](data => ({
    case o: JObject => (o \ "command").extract[String] match {
      case InitReceiver => InitReceiverCommand()
      case DestroyReceiver => DestroyReceiverCommand()
      case HandleMessage => o.extract[HandleMessageCommand]
      case GetReceiverName => GetReceiverNameCommand()
      case GetMappings => GetMappingsCommand()
    }
  }, {
    case irc: InitReceiverCommand => Extraction.decompose(irc)
    case drc: DestroyReceiverCommand => Extraction.decompose(drc)
    case hmc: HandleMessageCommand => Extraction.decompose(hmc)
    case grnc: GetReceiverNameCommand => Extraction.decompose(grnc)
    case gmc: GetMappingsCommand => Extraction.decompose(gmc)
  }))
}
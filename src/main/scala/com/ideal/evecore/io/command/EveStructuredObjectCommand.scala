package com.ideal.evecore.io.command

import com.ideal.evecore.interpreter.EveObject
import com.ideal.evecore.interpreter.remote.RemoteEveStructuredObjectMessage._
import org.json4s.{Extraction, CustomSerializer}
import org.json4s.JsonAST.JObject

/**
 * Created by Christophe on 27/03/2017.
 */
trait EveStructuredObjectCommand

case class GetTypeCommand(command: String = GetType) extends EveStructuredObjectCommand
case class GetFieldCommand(field: String, command: String = GetField) extends EveStructuredObjectCommand
case class SetFieldCommand(field: String, value: EveObject, command: String = SetField) extends EveStructuredObjectCommand
case class HasFieldCommand(field: String, command: String = HasField) extends EveStructuredObjectCommand
case class GetStateCommand(field: String, command: String = GetState) extends EveStructuredObjectCommand
case class SetStateCommand(field: String, value: String, command: String = SetState) extends EveStructuredObjectCommand
case class HasStateCommand(field: String, command: String = HasState) extends EveStructuredObjectCommand

object EveStructuredObjectCommand {
  //import com.ideal.evecore.io.Serializers.Formats

  implicit val EveStructuredObjectCommandSerializer = new CustomSerializer[EveStructuredObjectCommand](data => ({
    case o: JObject =>
      implicit val formats = data
      (o \ "command").extract[String] match {
        case GetType => GetTypeCommand()
        case GetField => o.extract[GetFieldCommand]
        case SetField => o.extract[SetFieldCommand]
        case HasField => o.extract[HasFieldCommand]
        case GetState => o.extract[GetStateCommand]
        case SetState => o.extract[SetStateCommand]
        case HasState => o.extract[HasStateCommand]
      }
  }, {
    case gtc: GetTypeCommand => Extraction.decompose(gtc)(data)
    case gfc: GetFieldCommand => Extraction.decompose(gfc)(data)
    case sfc: SetFieldCommand => Extraction.decompose(sfc)(data)
    case hfc: HasFieldCommand => Extraction.decompose(hfc)(data)
    case gsc: GetStateCommand => Extraction.decompose(gsc)(data)
    case ssc: SetStateCommand => Extraction.decompose(ssc)(data)
    case hsc: HasStateCommand => Extraction.decompose(hsc)(data)
    //case o => Extraction.decompose(o)(data)
  }))
}

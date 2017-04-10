package com.ideal.evecore.io.command

import ContextCommand._
import QueryCommand._
import org.json4s.{Extraction, CustomSerializer}
import org.json4s.JsonAST.JObject

/**
 * Created by Christophe on 27/03/2017.
 */
trait ContextCommand


case class FindOneItemOfTypeCommand(itemType: String, command: String = FindOneItemOfType) extends ContextCommand
case class FindItemsOfTypeCommand(itemType: String, command: String = FindItemsOfType) extends ContextCommand

object ContextCommand {
  import com.ideal.evecore.io.Serializers.Formats

  val ContextCommand = "CCMD"
  val FindItemsOfType = "FTYP"
  val FindOneItemOfType = "FOTY"

  implicit val ContextCommandSerializer = new CustomSerializer[ContextCommand](data => ({
    case o: JObject => (o \ "command").extract[String] match {
      case FindItemById => o.extract[FindItemByIdCommand]
      case FindOneItemOfType => o.extract[FindOneItemOfTypeCommand]
      case FindItemsOfType => o.extract[FindItemsOfTypeCommand]
      case ObjectRequest => o.extract[ObjectCommand]
    }
  }, {
    case fibic: FindItemByIdCommand => Extraction.decompose(fibic)
    case foiot: FindOneItemOfTypeCommand => Extraction.decompose(foiot)
    case fiot: FindItemsOfTypeCommand => Extraction.decompose(fiot)
    case oc: ObjectCommand => Extraction.decompose(oc)
  }))
}
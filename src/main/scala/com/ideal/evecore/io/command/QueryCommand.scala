package com.ideal.evecore.io.command

import QueryCommand._
/**
  * Created by Christophe on 10/04/2017.
  */
trait QueryCommand extends ContextCommand with ReceiverCommand

case class FindItemByIdCommand(id: String, command: String = FindItemById) extends QueryCommand
case class ObjectCommand(objectId: String, objectCommand: EveStructuredObjectCommand, command: String = ObjectRequest) extends QueryCommand

object QueryCommand {
  val FindItemById = "FBID"
  val ObjectRequest = "ORQT"
}

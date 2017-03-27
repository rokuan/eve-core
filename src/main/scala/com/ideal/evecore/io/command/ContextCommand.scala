package com.ideal.evecore.io.command

import com.ideal.evecore.interpreter.remote.RemoteContextMessage._

/**
 * Created by Christophe on 27/03/2017.
 */
trait ContextCommand

case class FindItemByIdCommand(id: String, command: String = FindItemById) extends ContextCommand
case class FindOneItemOfTypeCommand(itemType: String, command: String = FindOneItemOfType) extends ContextCommand
case class FindItemsOfTypeCommand(itemType: String, command: String = FindItemsOfType) extends ContextCommand
case class ObjectCommand(objectId: String, objectCommand: EveStructuredObjectCommand, command: String = ObjectRequest) extends ContextCommand

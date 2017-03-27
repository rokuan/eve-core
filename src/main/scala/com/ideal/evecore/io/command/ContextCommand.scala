package com.ideal.evecore.io.command

import com.ideal.evecore.interpreter.remote.RemoteContextMessage._

/**
 * Created by Christophe on 27/03/2017.
 */
trait ContextCommand

case class FindItemByIdCommand(id: String, command: String = FindItemById)
case class FindOneItemOfTypeCommand(itemType: String, command: String = FindOneItemOfType)
case class FindItemsOfTypeCommand(itemType: String, command: String = FindItemsOfType)
case class ObjectCommand(objectId: String, objectCommand: EveStructuredObjectCommand)

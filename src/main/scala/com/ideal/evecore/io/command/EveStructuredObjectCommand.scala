package com.ideal.evecore.io.command

import com.ideal.evecore.interpreter.EveObject
import com.ideal.evecore.interpreter.remote.RemoteEveStructuredObjectMessage._

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

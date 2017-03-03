package com.ideal.evecore.universe.receiver

import com.ideal.evecore.interpreter.{EveStructuredObject}
import com.rokuan.calliopecore.sentence.IAction.ActionType


sealed trait Message
case class ActionMessage(val action: ActionType) extends Message
case class EveObjectMessage(val obj: EveStructuredObject) extends Message

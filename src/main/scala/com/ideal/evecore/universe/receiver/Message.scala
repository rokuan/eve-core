package com.ideal.evecore.universe.receiver

import com.ideal.evecore.universe.route.ObjectValueSource
import com.rokuan.calliopecore.sentence.IAction.ActionType


sealed trait Message
case class ActionMessage(val action: ActionType) extends Message
case class ObjectMessage(val obj: ObjectValueSource) extends Message

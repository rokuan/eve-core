package com.ideal.evecore.universe.receiver

import com.rokuan.calliopecore.sentence.IAction.ActionType


sealed trait Message
case class ActionMessage(val action: ActionType) extends Message

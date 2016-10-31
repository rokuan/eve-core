package com.ideal.evecore.universe

import com.ideal.evecore.universe.route.{ObjectValueSource, ValueSource}
import com.rokuan.calliopecore.sentence.IAction
import com.rokuan.calliopecore.sentence.IAction.ActionType

/**
  * Created by Christophe on 24/07/2016.
  */
sealed trait EveMessage
case class ActionEveMessage(val action: ActionType, val values: List[ObjectValueSource]) extends EveMessage
case class EveObjectMessage(val action: IAction, val what: ValueSource, val how: ValueSource, val to: ValueSource) extends EveMessage

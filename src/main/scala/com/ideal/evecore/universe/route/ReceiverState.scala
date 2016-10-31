package com.ideal.evecore.universe.route

import com.ideal.evecore.universe.ValueMatcher
import com.ideal.evecore.universe.receiver.Receiver

import scala.collection.mutable.ListBuffer

/**
 * Created by Christophe on 16/09/2016.
 */
class ReceiverState extends State[Receiver] {
  val id = ReceiverState.newId()
  val nextStates = collection.mutable.LinkedHashMap[ValueMatcher, ListBuffer[State[Receiver]]]()

  def addNext(branch: ValueMatcher, next: State[Receiver]) = {
    nextStates.get(branch).map { states =>
      states += next
    }.getOrElse {
      nextStates.put(branch, new ListBuffer[State[Receiver]]() += next)
    }
  }

  override def getNext(): Set[(ValueMatcher, Seq[State[Receiver]])] = nextStates.toSet
}

object ReceiverState {
  private var stateId = 0
  private def newId(): Int = {
    val newId = stateId
    stateId += 1
    newId
  }
}

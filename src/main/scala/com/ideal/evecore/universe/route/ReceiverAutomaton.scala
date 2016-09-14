package com.ideal.evecore.universe.route

import com.ideal.evecore.universe.receiver.Receiver

/**
  * Created by Christophe on 15/07/2016.
  */
class ReceiverAutomaton extends Automaton[ObjectValueSource, Receiver] {
  override def add(o: Receiver): Unit = {

  }

  override def remove(o: Receiver): Unit = {}

  override def find(o: ObjectValueSource): Option[Receiver] = Option.empty[Receiver]
}

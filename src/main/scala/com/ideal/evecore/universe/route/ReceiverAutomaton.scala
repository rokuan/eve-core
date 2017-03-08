package com.ideal.evecore.universe.route

import com.ideal.evecore.interpreter.EveStructuredObject
import com.ideal.evecore.universe.receiver.Receiver

/**
  * Created by Christophe on 15/07/2016.
  */
class ReceiverAutomaton extends Automaton[EveStructuredObject, Receiver] {
  protected val receivers = collection.mutable.ListBuffer[Receiver]()

  override def add(o: Receiver): Unit = receivers += o

  override def remove(o: Receiver): Unit = receivers -= o

  override def find(o: EveStructuredObject): Option[Receiver] = receivers.find { r =>
    r.getMappings().forall { case (key, matcher) => o.get(key).map(matcher.matches).getOrElse(false) }
  }
}

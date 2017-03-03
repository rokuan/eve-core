package com.ideal.evecore.universe

import com.ideal.evecore.interpreter.EveStructuredObject
import com.ideal.evecore.universe.receiver.Receiver
import com.ideal.evecore.universe.route.ReceiverAutomaton

/**
 * Created by Christophe on 14/09/2016.
 */
trait World {
  def registerReceiver(receiver: Receiver): Unit
  def unregisterReceiver(receiver: Receiver): Unit
  //def findReceiver(o: ObjectValueSource): Option[Receiver]
  def findReceiver(o: EveStructuredObject): Option[Receiver]
}

class MinimalWorld extends World {
  private val receivers = collection.mutable.Map[String, Receiver]()
  private val automaton = new ReceiverAutomaton

  override def registerReceiver(receiver: Receiver): Unit = {
    val name = receiver.getReceiverName()
    receivers.get(name).map { r =>
      receivers.remove(name)
      automaton.remove(r)
      r.destroyReceiver()
    }
    receivers.put(name, receiver)
    automaton.add(receiver)
    receiver.initReceiver()
  }

  override def unregisterReceiver(receiver: Receiver): Unit = {
    receivers.remove(receiver.getReceiverName()).map { r =>
      automaton.remove(r)
      r.destroyReceiver()
    }
  }

  //override def findReceiver(o: ObjectValueSource): Option[Receiver] = automaton.find(o)
  override def findReceiver(o: EveStructuredObject): Option[Receiver] = automaton.find(o)
}

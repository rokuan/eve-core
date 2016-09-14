package com.ideal.evecore.universe

import com.ideal.evecore.universe.receiver.Receiver
import com.ideal.evecore.universe.route.{ReceiverAutomaton, ObjectValueSource}

/**
 * Created by Christophe on 14/09/2016.
 */
object World {
  private val receivers = collection.mutable.Map[String, Receiver]()
  private val automaton = new ReceiverAutomaton

  def registerReceiver(name: String, receiver: Receiver) = {
    receivers.get(name).map { r =>
      receivers.remove(name)
      automaton.remove(r)
      r.destroyReceiver()
    }
    receivers.put(name, receiver)
    automaton.add(receiver)
    receiver.initReceiver()
  }

  def unregisterReceiver(name: String) = {
    receivers.remove(name).map { r =>
      automaton.remove(r)
      r.destroyReceiver()
    }
  }

  def findReceiver(o: ObjectValueSource): Option[Receiver] = automaton.find(o)
}

package com.ideal.evecore.universe.receiver

import com.ideal.evecore.common.Mapping.Mapping
import com.ideal.evecore.interpreter.EveObject
import com.ideal.evecore.universe.ValueMatcher

import scala.util.Try


/**
  * Created by Christophe on 14/07/2016.
  */
trait Receiver {
  def initReceiver(): Unit
  def handleMessage(message: Message): Try[EveObject]
  def destroyReceiver(): Unit
  def getName(): String
  def getMappings(): Mapping[_ <: ValueMatcher]

  override def toString: String = getName()
  override def equals(obj: scala.Any): Boolean = obj match {
    case null => false
    case r: Receiver => Option(getName()).map(_.equals(r.getName())).getOrElse(false)
    case _ => false
  }
}

object Receiver {

}

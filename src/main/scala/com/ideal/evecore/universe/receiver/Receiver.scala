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
  def getReceiverName(): String = getClass.getName
  def getMappings(): Mapping[_ <: ValueMatcher]

  override def toString: String = getReceiverName()
  override def equals(obj: scala.Any): Boolean = obj match {
    case null => false
    case r: Receiver => Option(getReceiverName()).map(_.equals(r.getReceiverName())).getOrElse(false)
    case _ => false
  }
}
package com.ideal.evecore.interpreter

import com.ideal.evecore.interpreter.data.EveStructuredObject

/**
  * Created by Christophe on 05/02/2017.
  */
trait History {
  def addItem(o: EveStructuredObject): Unit
  def addPlace(o: EveStructuredObject): Unit
  def getLastItem(): Option[EveStructuredObject]
  def getLastItemOfType(t: String): Option[EveStructuredObject]
  def getLastPlace(): Option[EveStructuredObject]
}

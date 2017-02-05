package com.ideal.evecore.interpreter

/**
  * Created by Christophe on 05/02/2017.
  */
trait History {
  def addItem(o: EveObject): Unit
  def addPlace(o: EveObject): Unit
  def getLastItem(): Option[EveObject]
  def getLastItemOfType(t: String): Option[EveObject]
  def getLastPlace(): Option[EveObject]
}

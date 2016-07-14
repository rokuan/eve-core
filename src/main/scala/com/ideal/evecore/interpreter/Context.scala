package com.ideal.evecore.interpreter

import com.rokuan.calliopecore.sentence.structure.content.{INominalObject, IPlaceObject, ITimeObject}

import scala.util.Try

/**
  * Created by Christophe on 15/07/2016.
  */
trait Context[QueryType] {
  def addNominalObject(nominalObject: INominalObject)
  def addPlaceObject(placeObject: IPlaceObject)
  def addTimeObject(timeObject: ITimeObject)

  def findLastNominalObject(query: QueryType) : Try[INominalObject]
  def findLastPlaceObject(query: QueryType) : Try[IPlaceObject]
  def findLastTimeObject(query: QueryType) : Try[ITimeObject]
}

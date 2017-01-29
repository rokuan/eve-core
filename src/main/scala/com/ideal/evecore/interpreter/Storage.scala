package com.ideal.evecore.interpreter

import com.rokuan.calliopecore.sentence.structure.content.{ITimeObject, IWayObject, INominalObject}
import com.rokuan.calliopecore.sentence.structure.data.nominal._
import com.rokuan.calliopecore.sentence.structure.data.place.{PlaceObject, NamedPlaceObject, LocationObject, AdditionalPlace}
import com.rokuan.calliopecore.sentence.structure.data.time.{TimePeriodObject, DayPartObject, RelativeTimeObject, SingleTimeObject}
import com.rokuan.calliopecore.sentence.structure.data.time.TimeAdverbial.DateDefinition
import com.rokuan.calliopecore.sentence.structure.data.way.TransportObject

import scala.util.{Failure, Success, Try}

/**
  * Created by Christophe on 15/07/2016.
  */
trait Storage[QueryType] {
  import Storage._

  def set(context: Context[QueryType], left: INominalObject, field: String, value: INominalObject)
  def set(context: Context[QueryType], left: INominalObject, value: INominalObject)
  def delete(context: Context[QueryType], left: INominalObject, field: String, value: INominalObject)
  def delete(context: Context[QueryType], left: INominalObject, value: INominalObject)
}

object Storage {
  def notImplementedYet = Failure(new Exception("Not implemented yet"))
}
package com.ideal.evecore.interpreter

import com.rokuan.calliopecore.sentence.structure.content.INominalObject

import scala.util.Failure

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
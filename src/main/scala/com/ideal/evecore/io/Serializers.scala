package com.ideal.evecore.io

import org.json4s._

/**
 * Created by Christophe on 28/03/2017.
 */
object Serializers {
  implicit val Formats = DefaultFormats

  val ObjectIdKey = "__eve_id"
  val DomainIdKey = "__eve_domain_id"
  val NoneObjectString = "[__eve]None[/__eve]"
}

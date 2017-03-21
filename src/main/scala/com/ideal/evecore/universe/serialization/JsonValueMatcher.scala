package com.ideal.evecore.universe.serialization

import java.io.{InputStream, File}

import com.ideal.evecore.common.Mapping.Mapping
import com.ideal.evecore.universe._
import org.json4s.JsonAST._
import org.json4s.jackson.JsonMethods

import scala.io.Source

/**
 * Created by Christophe on 21/03/2017.
 */
object JsonValueMatcher {
  def parseMapping(f: File): Mapping[ValueMatcher] = {
    val jsonText = Source.fromFile(f).getLines().mkString
    parseMapping(jsonText)
  }

  def parseMapping(is: InputStream): Mapping[ValueMatcher] = {
    val jsonText = Source.fromInputStream(is).getLines().mkString
    parseMapping(jsonText)
  }

  def parseMapping(text: String): Mapping[ValueMatcher] = {
    val json = JsonMethods.parse(text)
    json match {
      case o: JObject => o
      case _ => Map[String, ValueMatcher]()
    }
  }

  private implicit def jObjectToObjectValueMatcher(o: JObject): Mapping[ValueMatcher] = o.obj.map { case (key, value) => (key -> apply(o)) }.toMap

  private def apply(o: JValue): ValueMatcher = o match {
    case o: JObject => new ObjectValueMatcher(o)
    case JString(s) => s
    case JBool(b) => b
    case JInt(i) => i
    case JDouble(d) => d
    case JArray(values) => OrValueMatcher(values.map(apply).toArray)
    case JNull => NullValueMatcher
    case _ => UndefinedValueMatcher
  }
}

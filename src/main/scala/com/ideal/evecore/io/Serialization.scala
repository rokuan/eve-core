package com.ideal.evecore.io

import java.net.Socket

import com.ideal.evecore.interpreter._
import com.ideal.evecore.interpreter.remote.RemoteEveStructuredObject
import com.ideal.evecore.io.message.{ResultWriter, ResultReader}
import org.json4s
import org.json4s.{Serializer, NoTypeHints, DefaultFormats, CustomSerializer, Extraction}
import org.json4s.JsonAST.JArray
import org.json4s.JsonAST.JBool
import org.json4s.JsonAST.JDouble
import org.json4s.JsonAST.JInt
import org.json4s.JsonAST.JNull
import org.json4s.JsonAST.JObject
import org.json4s.JsonAST.JString
import org.json4s.JsonAST.JValue

import org.json4s.jackson.Serialization

/**
 * Created by Christophe on 07/03/17.
 */


object Readers {
  implicit val format = DefaultFormats
  implicit val converters = Serialization.formats(NoTypeHints)

  implicit def booleanToJBool(b: java.lang.Boolean): JValue = JBool(b)
  implicit def numberToJDouble(n: java.lang.Number): JValue =
    if(n == math.floor(n.doubleValue())){
      JInt(n.intValue())
    } else {
      JDouble(n.doubleValue())
    }

  class EveObjectResultConverter(val socket: Socket) extends ResultReader[EveObject] with ResultWriter[EveObject] {
    implicit val converter: Serializer[EveObject] = new CustomSerializer[EveObject](data => ({
      case JString(s) => EveStringObject(s)
      case JDouble(d) => EveNumberObject(d)
      case JInt(i) => EveNumberObject(i)
      case JArray(l) => EveObjectList(l.map(_.extract[EveObject]))
      case JBool(b) => EveBooleanObject(b)
      case o: JObject => new RemoteEveStructuredObject((o \ "eve_id").extract[String], socket)
      case JNull => null
    }, {
      case EveStringObject(s) => JString(s)
      case EveBooleanObject(b) => JBool(b)
      case EveNumberObject(n) => n
      case EveObjectList(l) => JArray(l.map(Extraction.decompose(_)).toList)
      case null => JNull
    }))

    override def extract(o: JValue): EveObject = o.extract[EveObject]

    override def transform(o: EveObject): json4s.JValue = Extraction.decompose(o)
  }

  implicit object StringResultReader extends ResultReader[String] with ResultWriter[String] {
    override def extract(o: JValue): String = o.extract[String]

    override def transform(o: String): json4s.JValue = JString(o)
  }
}

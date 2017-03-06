package com.ideal.evecore.io.message

import java.io.InputStream
import java.net.Socket

import com.ideal.evecore.interpreter._
import com.ideal.evecore.interpreter.remote.RemoteEveStructuredObject
import org.json4s.JsonAST.{JNull, JObject}
import org.json4s.{CustomSerializer, DefaultFormats, JArray, NoTypeHints, Serializer}
import org.json4s.jackson.{JsonMethods, Serialization}
import org.json4s.JsonDSL._
import org.json4s._

/**
  * Created by Christophe on 05/03/2017.
  */
case class Result[T >: Null](success: Boolean, value: T)

trait ResultReader[T >: Null] {
  import Readers._
  /*implicit val formats = DefaultFormats
  implicit val caseFormats = Serialization.formats(NoTypeHints)*/

  private def readSize(is: InputStream) = {
    val sizeArray = new Array[Byte](4)
    if(is.read(sizeArray) != sizeArray.length){
      0
    } else {
      sizeArray.zipWithIndex
        .foldLeft(0){ case (acc, (size, index)) => acc + ((size & 0xFF) << (index * 8)) }
    }
  }

  final def readFrom(is: InputStream): Result[T] = {
    var size = readSize(is)
    val buffer = new StringBuffer()
    val data = new Array[Byte](1024)

    while(size > 0){
      val read = is.read(data)
      if(read > 0) {
        buffer.append(new String(data, 0, read))
      } else {
        size = 0
      }
    }

    val json = JsonMethods.parse(buffer.toString)
    val success = (json \ "success").extractOpt[Boolean].getOrElse(false)

    if(success){
      Result[T](true, extract(json \ "value"))
    } else {
      Result[T](false, null)
    }
  }

  def extract(o: JValue): T
}

object Readers {
  implicit val format = DefaultFormats
  implicit val converters = Serialization.formats(NoTypeHints)

  //implicit def stringToJString(s: String) = Option(s).map(JString).getOrElse(JNull)
  implicit def booleanToJBool(b: java.lang.Boolean): JValue = JBool(b)
  implicit def numberToJDouble(n: java.lang.Number): JValue =
    if(n == math.floor(n.doubleValue())){
      JInt(n.intValue())
    } else {
      JDouble(n.doubleValue())
    }

  //implicit object BooleanSerializer ex

  class EveObjectResultReader(val socket: Socket) extends ResultReader[EveObject] {
    //override implicit val formats = DefaultFormats + reader
    implicit val reader: Serializer[EveObject] = new CustomSerializer[EveObject](data => ({
      case o: JObject => (o \ "eveType").extract[String] match {
        case "string" => EveStringObject((o \ "value").extract[String])
        case "number" => EveNumberObject((o \ "value").extract[Number])
        case "boolean" => EveBooleanObject((o \ "value").extract[Boolean])
        case "list" => EveObjectList((o \ "value").asInstanceOf[JArray].arr.map { element => element.extract[EveObject] })
        case "object" => new RemoteEveStructuredObject((o \ "eve_id").extract[String], socket)
        case _ => null
      }
      case JNull => null
    }, {
      case s: EveStringObject => ("eveType" -> "string") ~ ("value" -> s.s)
      case b: EveBooleanObject => ("eveType" -> "boolean") ~ ("value" -> b.b)
      case n: EveNumberObject => ("eveType" -> "number") ~ ("value" -> n.n)
      case l: EveObjectList => ("eveType" -> "list") ~ ("value" -> JArray(l.a.map(Extraction.decompose(_)).toList))
    }))

    override def extract(o: JValue): EveObject = o.extract[EveObject]
  }

  implicit object StringResultReader extends ResultReader[String] {
    override def extract(o: JValue): String = o.extract[String]
  }
}

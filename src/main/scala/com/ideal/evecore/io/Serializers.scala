package com.ideal.evecore.io

import com.ideal.evecore.interpreter._
import com.ideal.evecore.interpreter.remote.RemoteEveStructuredObject
import com.ideal.evecore.io.command.{EveStructuredObjectCommand, ContextCommand, ReceiverCommand, UserCommand}
import com.ideal.evecore.io.message.Result
import org.json4s
import org.json4s.JsonAST.JArray
import org.json4s.JsonAST.JBool
import org.json4s.JsonAST.JDouble
import org.json4s.JsonAST.JInt
import org.json4s.JsonAST.JNull
import org.json4s.JsonAST.JObject
import org.json4s.JsonAST.JString
import org.json4s.JsonAST.JValue
import org.json4s._
import org.json4s.JsonDSL._
import org.json4s.jackson.Serialization

/**
 * Created by Christophe on 28/03/2017.
 */
object Serializers {
  implicit val Formats = DefaultFormats

  val ObjectIdKey = "__eve_id"
  val DomainIdKey = "__eve_domain_id"
  val NoneObjectString = "[__eve]None[/__eve]"

  def buildBasicFormats() = DefaultFormats + UserCommand.UserCommandSerializer + EveStructuredObjectCommand.EveStructuredObjectCommandSerializer +
    ReceiverCommand.ReceiverCommandSerializer + ContextCommand.ContextCommandSerializer

  def buildRemoteFormats(handler: StreamHandler, id: String = "UNKNOWN") = buildBasicFormats() +
    new EveObjectSerializer(id, handler) +
    new EveStructuredObjectSerializer(id, handler) +
    new EveObjectListSerializer(id, handler) /*+
    new ResultSerializer[EveObject]() +
    new ResultSerializer[EveStructuredObject]() +
    new ResultSerializer[EveObjectList]() +
    new ResultSerializer[String]()*/

  implicit def numberToJDouble(n: java.lang.Number): JValue =
    if(n == math.floor(n.doubleValue())){
      JInt(n.intValue())
    } else {
      JDouble(n.doubleValue())
    }

  trait ObjectConverter[T] {
    def writeObject(o: T): JObject
    def readObject(o: JObject): T
  }

  def extractObject(o: JObject, handler: StreamHandler)(implicit formats: Formats): EveStructuredObject = (o \ "type").extract[String] match {
    case "MAPPING" =>
      val content = (o \ "value").asInstanceOf[JObject]
      EveMappingObject(content.obj.map { case (key, value) => (key -> value.extract[EveObject]) }.toMap)
    case "REMOTE" => {
      val sourceId = (o \ DomainIdKey).extract[String]
      val objectId = (o \ ObjectIdKey).extract[String]
      new RemoteEveStructuredObject(sourceId, objectId, handler)
    }
  }
  def extractList(o: JArray)(implicit formats: Formats): EveObjectList = EveObjectList(o.arr.map(_.extract[EveObject]))

  def decomposeObject(o: EveStructuredObject, domainId: String)(implicit formats: Formats): JValue = o match {
    case o: EveQueryObject =>
      ("type" -> JString("REMOTE")) ~ (ObjectIdKey -> o.id) ~ (DomainIdKey -> domainId)
    case EveMappingObject(m) =>
      ("type" -> "MAPPING") ~
        ("value" -> JObject(m.map { case (key, value) =>  (key -> Extraction.decompose(value)) }.toList))
    case o: RemoteEveStructuredObject =>
      ("type" -> JString("REMOTE")) ~
        (ObjectIdKey -> o.objectId) ~
        (DomainIdKey -> o.domainId)
  }
  def decomposeList(o: EveObjectList)(implicit formats: Formats): JValue = JArray(o.a.map(Extraction.decompose(_)).toList)

  class EveObjectSerializer(val domainId: String, val handler: StreamHandler) extends CustomSerializer[EveObject](data => ({
    case JString(s) if s == NoneObjectString => NoneObject
    case JString(s) => s
    case JDouble(d) => EveNumberObject(d)
    case JInt(i) => EveNumberObject(i)
    //case JArray(l) => EveObjectList(l.map(_.extract[EveObject]))
    case a: JArray => extractList(a)(data)
    case JBool(b) => EveBooleanObject(b)
    /*case o: JObject => (o \ "type").extract[String] match {
      case "MAPPING" =>
        val content = (o \ "value").asInstanceOf[JObject]
        EveMappingObject(content.obj.map { case (key, value) => (key -> value.extract[EveObject]) }.toMap)
      case "REMOTE" => {
        val sourceId = (o \ DomainIdKey).extract[String]
        val objectId = (o \ ObjectIdKey).extract[String]
        new RemoteEveStructuredObject(sourceId, objectId, handler)
      }
    }*/
    case o: JObject => extractObject(o, handler)(data)
    case JNull => null
  }, {
    case NoneObject => JString(NoneObjectString)
    /*case o: EveQueryObject =>
      ("type" -> JString("REMOTE")) ~ (ObjectIdKey -> o.id) ~ (DomainIdKey -> domainId)
    case EveMappingObject(m) =>
      ("type" -> "MAPPING") ~
        ("value" -> JObject(m.map { case (key, value) =>  (key -> Extraction.decompose(value)) }.toList))
    case o: RemoteEveStructuredObject =>
      ("type" -> JString("REMOTE")) ~
        (ObjectIdKey -> o.objectId) ~
        (DomainIdKey -> o.domainId)*/
    case o: EveStructuredObject => decomposeObject(o, domainId)
    case EveStringObject(s) => JString(s)
    case EveBooleanObject(b) => JBool(b)
    case EveNumberObject(n) => n
    case d: EveDateObject => JString(d.toFormattedString())
    case EveObjectList(l) => JArray(l.map(Extraction.decompose(_)).toList)
    case null => JNull
  }))

  class EveStructuredObjectSerializer(val domainId: String, val handler: StreamHandler) extends CustomSerializer[EveStructuredObject](data => ({
    case o: JObject => {
      /*implicit val formats = data + new EveObjectSerializer(domainId, handler)
      o.extract[EveObject](formats, manifest[EveObject]).asInstanceOf[EveStructuredObject]*/
      //o.extract[EveObject].asInstanceOf[EveStructuredObject]
      extractObject(o, handler)
    }
  }, {
    case o: EveStructuredObject => {
      /*implicit val formats = data + new EveObjectSerializer(domainId, handler)
      Extraction.decompose(o)(formats)*/
      //Extraction.decompose(o.asInstanceOf[EveObject])
      decomposeObject(o, domainId)
    }
  }))

  class EveObjectListSerializer(val domainId: String, val handler: StreamHandler) extends CustomSerializer[EveObjectList](data => ({
    case o: JArray => {
      /*implicit val formats = data + new EveObjectSerializer(domainId, handler)
      o.extract[EveObject](formats, manifest[EveObject]).asInstanceOf[EveObjectList]*/
      //o.extract[EveObject].asInstanceOf[EveObjectList]
      extractList(o)
    }
  }, {
    case o: EveObjectList => {
      /*implicit val formats = data + new EveObjectSerializer(domainId, handler)
      Extraction.decompose(o)(formats)*/
      //Extraction.decompose(o.asInstanceOf[EveObject])
      decomposeList(o)
    }
  }))

  class ResultSerializer[T >: Null](implicit m: Manifest[T]) extends CustomSerializer[Result[T]](formats => ({
    case o: JObject => {
      val success = (o \ "success").extractOpt[Boolean].getOrElse(false)
      if(success){
        Result.Ok[T]((o \ "value").extract[T])
      } else {
        Result.Ko[T]((o \ "error").extract[String])
      }
    }
  }, {
    case r: Result[T] => {
      if (r.success) {
        ("success" -> JBool(true)) ~ ("value" -> Extraction.decompose(r.value))
      } else {
        ("success" -> JBool(false)) ~ ("error" -> r.error)
      }
    }
  }))
}

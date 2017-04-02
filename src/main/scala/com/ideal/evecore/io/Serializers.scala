package com.ideal.evecore.io

import java.net.Socket

import com.ideal.evecore.interpreter._
import com.ideal.evecore.interpreter.remote.RemoteEveStructuredObject
import com.ideal.evecore.io.command.{ContextCommand, ReceiverCommand, UserCommand}
import com.ideal.evecore.io.message.Result
import org.json4s.{Extraction, CustomSerializer, DefaultFormats}
import org.json4s.JsonAST._
import org.json4s.JsonDSL._

/**
 * Created by Christophe on 28/03/2017.
 */
object Serializers {
  implicit val Formats = DefaultFormats

  val ObjectIdKey = "__eve_id"
  val DomainIdKey = "__eve_domain_id"
  val NoneObjectString = "[__eve]None[/__eve]"

  def buildBasicFormats() = DefaultFormats + UserCommand.UserCommandSerializer +
    ReceiverCommand.ReceiverCommandSerializer + ContextCommand.ContextCommandSerializer

  def buildRemoteFormats(id: String, handler: SocketLockHandler) = buildBasicFormats() + new EveObjectSerializer(id, handler) + new ResultSerializer[EveObject]()

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

  class EveObjectSerializer(val domainId: String, val handler: SocketLockHandler) extends CustomSerializer[EveObject](data => ({
    case JString(s) if s == NoneObjectString => NoneObject
    case JString(s) => s
    case JDouble(d) => EveNumberObject(d)
    case JInt(i) => EveNumberObject(i)
    case JArray(l) => EveObjectList(l.map(_.extract[EveObject]))
    case JBool(b) => EveBooleanObject(b)
    case o: JObject => (o \ "objectType").extract[String] match {
      case "mapping" => EveMappingObject(o.obj.map { case (key, value) => (key -> value.extract[EveObject]) }.toMap)
      case "query" => {
        val sourceId = (o \ "__eve_domain_id").extract[String]
        val objectId = (o \ ObjectIdKey).extract[String]
        new RemoteEveStructuredObject(sourceId, objectId, handler)
      }
      case "remote" => new RemoteEveStructuredObject(domainId, (o \ ObjectIdKey).extract[String], handler)
    }
    case JNull => null
  }, {
    case NoneObject => JString(NoneObjectString)
    case o: EveQueryObject =>
      ("objectType" -> JString("query")) ~ (ObjectIdKey -> o.id) ~ ("__eve_domain_id" -> domainId)
    case EveMappingObject(m) =>
      ("objectType" -> "mapping") ~
        JObject(m.map { case (key, value) =>  (key -> Extraction.decompose(value)) }.toList)
    case o: RemoteEveStructuredObject =>
      ("objectType" -> JString("remote")) ~
        (ObjectIdKey -> o.objectId) ~
        (DomainIdKey -> o.domainId)
    case EveStringObject(s) => JString(s)
    case EveBooleanObject(b) => JBool(b)
    case EveNumberObject(n) => n
    case d: EveDateObject => JString(d.toFormattedString())
    case EveObjectList(l) => JArray(l.map(Extraction.decompose(_)).toList)
    case null => JNull
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

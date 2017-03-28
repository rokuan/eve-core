package com.ideal.evecore.io

import java.net.Socket

import com.ideal.evecore.interpreter.remote.RemoteEveStructuredObject
import com.ideal.evecore.interpreter._
import com.ideal.evecore.universe.receiver.EveObjectMessage
import org.json4s.{Extraction, CustomSerializer, DefaultFormats}
import org.json4s.JsonAST._

/**
 * Created by Christophe on 28/03/2017.
 */
object Serializers {
  implicit val Formats = DefaultFormats

  implicit def numberToJDouble(n: java.lang.Number): JValue =
    if(n == math.floor(n.doubleValue())){
      JInt(n.intValue())
    } else {
      JDouble(n.doubleValue())
    }

  class MessageSerializer(val socket: Socket) extends CustomSerializer[EveObjectMessage](data => ({
    case o: JObject => new EveObjectMessage(o.extract[EveObject].asInstanceOf[EveStructuredObject])
  }, {
    case EveObjectMessage(o) => Extraction.decompose(o)
  }))

  trait ObjectConverter[T] {
    def writeObject(o: T): JObject
    def readObject(o: JObject): T
  }

  class RemoteEveObjectSerializer(socket: Socket) extends EveObjectSerializer(socket) {
    implicit val converter = new ObjectConverter[EveStructuredObject] {
      override def writeObject(o: EveStructuredObject): JObject = {

      }

      override def readObject(o: JObject): EveStructuredObject = {

      }
    }
  }

  class StreamEveObjectSerializer(contextId: String, socket: Socket) extends EveObjectSerializer(socket) {

  }

  abstract class EveObjectSerializer(val socket: Socket)(implicit c: ObjectConverter[EveStructuredObject]) extends CustomSerializer[EveObject](data => ({
    case JString(s) => s
    case JDouble(d) => EveNumberObject(d)
    case JInt(i) => EveNumberObject(i)
    case JArray(l) => EveObjectList(l.map(_.extract[EveObject]))
    case JBool(b) => EveBooleanObject(b)
    /*case o: JObject => {
      (o \ EveObject.IdKey).extractOpt[String].map { id =>
        new RemoteEveStructuredObject(contextId, id, socket)
      }.getOrElse(JValueConverters.jObjectToEveStructuredObject(o))
    }*/
    case o: JObject => c.readObject(o)
    case JNull => null
  }, {
    case EveMappingObject(m) => JObject(m.map { case (key, value) =>  (key -> Extraction.decompose(value)) }.toList)
    /*case o: EveStructuredObject => o.get(EveObject.IdKey).collect {
      case EveStringObject(id) => JObject(EveObject.IdKey -> JString(id))
    }.getOrElse(JObject())*/
    case o: EveStructuredObject => c.writeObject(o)
    case EveStringObject(s) => JString(s)
    case EveBooleanObject(b) => JBool(b)
    case EveNumberObject(n) => n
    case d: EveDateObject => JString(d.toFormattedString())
    case EveObjectList(l) => JArray(l.map(Extraction.decompose(_)).toList)
    case null => JNull
  }))
}

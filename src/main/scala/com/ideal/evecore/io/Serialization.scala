package com.ideal.evecore.io

import java.io.{OutputStream, InputStream}
import java.net.Socket

import com.ideal.evecore.common.Mapping
import com.ideal.evecore.common.Mapping.Mapping
import com.ideal.evecore.interpreter._
import com.ideal.evecore.interpreter.remote.RemoteEveStructuredObject
import com.ideal.evecore.io.Readers.{ValueMatcherConverter, EveObjectResultConverter}
import com.ideal.evecore.io.message.{ResultWriter, ResultReader}
import com.ideal.evecore.universe._
import com.ideal.evecore.universe.receiver.{EveObjectMessage, ActionMessage, Message}
import com.rokuan.calliopecore.sentence.IAction.ActionType
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
import org.json4s.JsonDSL._

import org.json4s.jackson.{JsonMethods, Serialization}

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

  implicit object ValueMatcherConverter extends ResultReader[ValueMatcher] with ResultWriter[ValueMatcher] {
    import ValueMatcher._

    implicit val converter: Serializer[ValueMatcher] = new CustomSerializer[ValueMatcher](data => ({
      case JString(s) => s
      case JBool(b) => b
      case JInt(i) => i
      case JDouble(d) => d
      case JArray(a) => a.map(_.extract[ValueMatcher]).toArray[ValueMatcher]
      case JObject(o) => o.map { case (key, value) => (key -> value.extract[ValueMatcher]) }.toMap
      case JNull => null
      case _ => UndefinedValueMatcher
    }, {
      case StringValueMatcher(s) => JString(s)
      case BooleanValueMatcher(b) => JBool(b)
      case NumberValueMatcher(n) => n
      case OrValueMatcher(a) => JArray(a.map(Extraction.decompose(_)).toList)
      case ObjectValueMatcher(m) => JObject(m.map { case (key, value) => (key -> Extraction.decompose(value)) }.toList)
    }))

    override def extract(o: json4s.JValue): ValueMatcher = o.extract[ValueMatcher]

    override def transform(o: ValueMatcher): json4s.JValue = Extraction.decompose(o)
  }

  class MessageConverter(val socket: Socket) extends ResultReader[Message] with ResultWriter[Message] {
    implicit val eveObjectConvert = new EveObjectResultConverter(socket)
    implicit val converter = new CustomSerializer[Message](data => ({
      case o: JObject => ((o \ "type").extract[String]) match {
        case "action" => new ActionMessage(ActionType.valueOf((o \ "action").extract[String]))
        case "object" => new EveObjectMessage((o \ "content").extract[EveObject].asInstanceOf[EveStructuredObject])
      }
    }, {
      case ActionMessage(a) => ("type" -> "action") ~ ("action" -> a.name())
      case EveObjectMessage(o) => ("type" -> "object") ~ ("content" -> Extraction.decompose(o))
    }))

    override def extract(o: json4s.JValue): Message = o.extract[Message]

    override def transform(o: Message): json4s.JValue = Extraction.decompose(o)
  }

  implicit object StringResultConverter extends ResultReader[String] with ResultWriter[String] {
    override def extract(o: JValue): String = o.extract[String]
    override def transform(o: String): json4s.JValue = JString(o)
  }
}

trait StreamReader[T] {
  def readFrom(is: InputStream): T
}

trait StreamWriter[T] {
  def writeTo(os: OutputStream, o: T): Unit
}

trait StreamHandler[T] extends StreamReader[T] with StreamWriter[T]

object Streamers {
  def readSize(is: InputStream): Int = {
    val data = new Array[Byte](4)
    if(is.read(data) != 1) {
      data.zipWithIndex.foldLeft(0) { case (acc, (size, index)) => acc + ((size & 0xFF) << (index * 8)) }
    } else {
      0
    }
  }

  def writeSize(os: OutputStream, size: Int) = {
    val sizeData = new Array[Byte](4)
    (0 until sizeData.length).foreach(index => sizeData(index) = ((size >> (index * 8)) & 0xFF).toByte)
    os.write(sizeData)
    os.flush()
  }

  implicit object BooleanStreamHandler extends StreamHandler[Boolean] {
    override def writeTo(os: OutputStream, o: Boolean): Unit = {
      os.write(if(o){ 1 } else { 0 })
      os.flush()
    }

    override def readFrom(is: InputStream): Boolean = (is.read() != 0)
  }

  implicit object StringStreamHandler extends StreamHandler[String] {
    override def writeTo(os: OutputStream, o: String): Unit = {
      val bytes = o.getBytes
      writeSize(os, bytes.length)
      os.write(bytes)
      os.flush()
    }

    override def readFrom(is: InputStream): String = {
      var size = readSize(is)

      if(size >= 0){
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

        buffer.toString
      } else {
        null
      }
    }
  }

  implicit object MessageStreamHandler extends StreamHandler[Message] {
    override def writeTo(os: OutputStream, o: Message): Unit = ???

    override def readFrom(is: InputStream): Message = ???
  }

  implicit object ValueMatcherMappingStreamHandler extends StreamHandler[Mapping[ValueMatcher]] {
    override def writeTo(os: OutputStream, o: Mapping[ValueMatcher]): Unit = {
      val json = JObject(o.map { case (key, value) => (key -> ValueMatcherConverter.transform(value)) }.toSeq: _*)
      StringStreamHandler.writeTo(os, JsonMethods.compact(json))
    }

    override def readFrom(is: InputStream): Mapping[ValueMatcher] = {
      val json = StringStreamHandler.readFrom(is)
      Option(json).map(JsonMethods.parse(_))
        .collect { case JObject(vs) => vs.map { case (key, value) => (key -> ValueMatcherConverter.extract(value)) }.toMap }
        .getOrElse(Mapping[ValueMatcher]())
    }
  }

  class EveObjectStreamHandler(val socket: Socket) extends StreamHandler[EveObject] {
    private val converter = new EveObjectResultConverter(socket)

    override def writeTo(os: OutputStream, o: EveObject): Unit = {
      val json = converter.transform(o)
      StringStreamHandler.writeTo(os, JsonMethods.compact(json))
    }

    override def readFrom(is: InputStream): EveObject = {
      val json = StringStreamHandler.readFrom(is)
      Option(json).map(JsonMethods.parse(_))
        .map(converter.extract)
        .orNull
    }
  }
}

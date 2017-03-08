import org.json4s.JString
import org.json4s.jackson.JsonMethods
import org.scalatest.{Matchers, FlatSpec}
import org.json4s.jackson.JsonMethods._

/**
 * Created by Christophe on 07/03/17.
 */
class JValueTest extends FlatSpec with Matchers {
  "This test" should "convert the string to a JString" in {
    //val element = "\"bonjour\""
    //val element = "{\"hello\": \"bonjour\", \"size\": 4, parse: false}"
    val element = """{ "hello": "bonjour", "size": 4, "parse": false }"""
    val json = parse(element)
    println(json)
    println(compact(json))
    //assert(json.isInstanceOf[JString])
  }
}

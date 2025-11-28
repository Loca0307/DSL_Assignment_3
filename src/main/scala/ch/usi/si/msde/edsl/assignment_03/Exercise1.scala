package ch.usi.si.msde.edsl.assignment_03

import scala.language.implicitConversions

import model.*
import model.HttpRequestModel.*
import model.JsonModel.*
import model.AsyncContext
import scala.concurrent.Future
import scala.collection.mutable.ArrayBuffer

object JsonDSL:

  // Do not touch this.
  def jobj(values: (String, JsonValue)*) = JsonObject(List(values*))

  // Implement the rest of the DSL below.

  def jarray(values: JsonValue*): JsonArray = JsonArray(List(values*))

  /* 
   Convert raw types to JsonValues
   */
  given Conversion[String, JsonValue] =   // Works also with JsonString but for clearity and consistency for extension we leave JsonValue
    (str: String) => JsonString(str)

  given Conversion[Boolean, JsonValue] =  // Not [Boolean, JsonBoolean} otherwise the extension/infix cannot properly "cast" types
    (boo: Boolean) =>
      boo match
        case true => JsonTrue
        case false => JsonFalse

  given Conversion[Double, JsonValue] =
    (dou: Double) => JsonNumber(dou)

  given Conversion[Int, JsonValue] =
    (int: Int) => JsonNumber(int.toDouble)

  given Conversion[Null, JsonValue] =
    _ => JsonNull

  /* 
  We cannot do something like
  case object key:
    type ValueType = JsonString
    def `:`(value: JsonValue): JsonString = JsonValue
  because all methods that end with a colon (:) are right-associative (Lecture 7, slide 13).
  In fact:
  a : b is equivalent to b.:(a)
  In our case "name" `:` "morpheus" is parsed by Scala as "morpheus".`:("name").
  We have to declare `:` using extension and use the infix modifier to swap the value and the key.
  We are "attaching" a method `:` in String/JsonString
  */
  extension (key: String)
    infix def `:`(value: JsonValue): (String, JsonValue) =
      (key, value)

end JsonDSL

@main def exercise1() =
  // DO NOT touch this
  import JsonDSL.{given, *}

  // Uncomment the lines below.

  // an empty json object.
  val emptyJson = jobj()

  /** An object with key-pairs, equivalent to:
    * { "name": "morpheus", "job": "leader" }
    */
  val jsonFragment1 = jobj(
    "name" `:` "morpheus",
    "job" `:` "leader"
  )

  /** A more complex example **/
  val jsonFragment2 = jobj(
    "title" `:` "The Matrix",
    "isSequel" `:` false,
    "duration" `:` 136,
    "directors" `:` jarray(jobj(
      "firstName" `:` "Lana",
      "lastName" `:` "Wachowski"
    ), jobj(
      "firstName" `:` "Lilly",
      "lastName" `:` "Wachowski"
    )),
    "starring" `:` jarray(
      "Keanu Reeves", "Laurence Fishburne", "Carrie-Ann Moss"
    ),
    "prequel" `:` null
  )

  println(emptyJson)
  println(jsonFragment1)
  println(jsonFragment2)

end exercise1

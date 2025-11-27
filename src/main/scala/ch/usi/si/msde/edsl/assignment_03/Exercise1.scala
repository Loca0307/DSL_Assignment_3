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

end JsonDSL

@main def exercise1() =
  // DO NOT touch this
  import JsonDSL.{given, *}

  // Uncomment the lines below.

  // // an empty json object.
  // val emptyJson = jobj()

  // /** An object with key-pairs, equivalent to:
  //   * { "name": "morpheus", "job": "leader" }
  //   */
  // val jsonFragment1 = jobj(
  //   "name" `:` "morpheus",
  //   "job" `:` "leader"
  // )

  // /** A more complex example **/
  // val jsonFragment2 = jobj(
  //   "title" `:` "The Matrix",
  //   "isSequel" `:` false,
  //   "duration" `:` 136,
  //   "directors" `:` jarray(jobj(
  //     "firstName" `:` "Lana",
  //     "lastName" `:` "Wachowski"
  //   ), jobj(
  //     "firstName" `:` "Lilly",
  //     "lastName" `:` "Wachowski"
  //   )),
  //   "starring" `:` jarray(
  //     "Keanu Reeves", "Laurence Fishburne", "Carrie-Ann Moss"
  //   ),
  //   "prequel" `:` null
  // )

  // println(emptyJson)
  // println(jsonFragment1)
  // println(jsonFragment2)

end exercise1

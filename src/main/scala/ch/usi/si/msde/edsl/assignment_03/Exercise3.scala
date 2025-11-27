package ch.usi.si.msde.edsl.assignment_03

import scala.language.implicitConversions

import scala.concurrent.Future
import scala.util.{Try, Success, Failure}
import scala.util.Success
import model.HttpRequestModel.*
import model.AssertionModel.*
import model.AssertionExecutor
import scala.concurrent.Await
import ch.usi.si.msde.edsl.assignment_03.model.JsonModel.JsonObject
import ch.usi.si.msde.edsl.assignment_03.model.JsonModel.JsonValue

trait RequestAssertionDSL extends AssertionExecutor:

  // Do not touch this import
  import model.AsyncContext.*

  // Write your DSL below

end RequestAssertionDSL

class Exercise3_Example extends RequestAssertionDSL:

  // Do not touch this
  import JsonDSL.{given, *}
  import HttpRequestDSL.{given, *}
  import model.AsyncContext.{given, *}

  // Exercise 3: respond/fail assertions
  // uncomment below

  // "a get on user 1 with api key" should "respond with 200 ok" += eventually (
  //   (https `://` "reqres.in" / "api" / "user" / "1").GET `with` headers == (
  //     "x-api-key" -> "reqres-free-v1"
  //   )
  // ) should respond `with` statusCode(200)

  // "a get on user 1 without api key" should "respond with 200 or 401" += eventually (
  //   (https `://` "reqres.in" / "api" / "user" / "1").GET
  // ) should respond `with` statusCode(200) | statusCode(401)

  // "a get on user 3 with api key" should "respond with some json" += eventually (
  //   (https `://` "reqres.in" / "api" / "user" / "3").GET `with` headers == (
  //     "x-api-key" -> "reqres-free-v1"
  //   )
  // ) should respond `with` contentType("application/json")

  // "a get on a non-existing user" should "respond with 404" += eventually (
  //   (https `://` "reqres.in" / "api" / "user" / "-3721").GET `with` headers == (
  //     "x-api-key" -> "reqres-free-v1"
  //   )
  // ) should respond `with` statusCode(404) & contentType("application/json")

  // "a GET on a non-existing domain" should "fail" += eventually (
  //   (https `://` "www.usi.chh").GET
  //  ) should fail

  // "a login without password" should "respond with contentType json" += eventually (
  //   (https `://` "reqres.in" / "api" / "login").POST `with` headers == (
  //     "x-api-key" -> "reqres-free-v1"
  //   ) and body == jobj(
  //     "email" `:` "morpheus@nebuchadnezzar"
  //   )
  // ) should respond `with` contentType("application/json")

  // "a login without password" should "respond with a json body containing an error" += eventually (
  //   (https `://` "reqres.in" / "api" / "login").POST `with` headers == (
  //     "x-api-key" -> "reqres-free-v1"
  //   ) and body == jobj(
  //     "email" `:` "morpheus@nebuchadnezzar"
  //   )
  // ) should respond `with` responseBody(jobj(
  //   "error" `:` "Missing password"
  // )) & statusCode(400)

  // do ** NOT ** remove this
  run()
end Exercise3_Example

@main def exercise3 = Exercise3_Example()

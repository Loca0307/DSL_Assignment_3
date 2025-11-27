package ch.usi.si.msde.edsl.assignment_03

import scala.language.implicitConversions

import model.*
import model.HttpRequestModel.*
import model.JsonModel.*
import model.AsyncContext
import scala.concurrent.Future
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

object HttpRequestDSL:

  // ** Do not touch this **
  import JsonDSL.*
  import AsyncContext.{given, *}

  // ** Implement the DSL below **

end HttpRequestDSL

@main def exercise2() =

  // DO NOT touch this
  import JsonDSL.{given, *}
  import HttpRequestDSL.{given, *}
  import AsyncContext.{given, *}

  // Exercise 2, Uncomment below
  // val getRequest1: Future[Response] =
  //   (https `://` "www.usi.ch" / "en" / "university").GET

  // val getRequest2 =
  //   (https `://` "reqres.in" / "api" / "user" / "1").GET `with` headers == (
  //     "x-api-key" -> "reqres-free-v1"
  //   )

  // val getRequest3 = (http `://` "usi.cch").GET

  // val postRequest1 =
  //   (https `://` "reqres.in" / "api" / "users").POST `with` body == jobj(
  //     "name" `:` "morpheus",
  //     "job" `:` "leader"
  //   ) and headers == (
  //     "x-api-key" -> "reqres-free-v1"
  //   )

  // val postRequest2 =
  //   (https `://` "reqres.in" / "api" / "register").POST `with` body == jobj(
  //     "email" `:` "agent.smith@reqres.in",
  //     "password" `:` "OguhGnivaew"
  //   ) and headers == (
  //     "x-api-key" -> "reqres-free-v1"
  //   )

  // val postRequest3 =
  //   (https `://` "reqres.in" / "api" / "login").POST `with` headers == (
  //     "x-api-key" -> "reqres-free-v1"
  //   )  and body == jobj(
  //     "email" `:` "morpheus@nebuchadnezzar"
  //   )

  // Do not touch this, just uncomment below.
  // executeInSequence(
  //   getRequest1,
  //   getRequest2,
  //   getRequest3,
  //   postRequest1,
  //   postRequest2,
  //   postRequest3
  // )
end exercise2

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

  // Expose schemes (entry points)
  val http = URLScheme.HTTP
  val https = URLScheme.HTTPS

  // URL builder 
  case class UrlBuilder(
    scheme: URLScheme,
    domainAndPath: DomainAndPath
  ):
    def build: URL = URL(scheme, domainAndPath)

  /**
    * Scheme `://` 
    */ 
  extension (s: URLScheme)
    // Parse http `://` "usi.cch" as http.`://`("usi.cch") and return UrlBuilder(HTTPS, DomainAndPath(List("www.usi.ch"))).
    infix def `://` (domain: String): UrlBuilder = 
      UrlBuilder(s, DomainAndPath(List(domain)))
      
    // Parse https `://` "www.usi.ch" / "en" / "university" as 
    // https.`://`("www.usi.ch", "en", "university")
    // and return UrlBuilder(HTTPS, DomainAndPath(List("www.usi.ch", "en", "university"))).
    infix def `://` (dap: DomainAndPath): UrlBuilder = 
      UrlBuilder(s, dap)

  /**
    * This works with entry points (https `://` "www.usi.ch")
    */
  extension (domain: String)
    infix def / (segment: String): DomainAndPath = 
      DomainAndPath(List(domain, segment))

  /**
    * This works with the next part of the path  (/ "en" / ...)
    */
  extension (d: DomainAndPath)
    infix def / (segment: String): DomainAndPath = 
      d.append(segment)



  /* 
   GET builder and conversion to Future
   */
  case class GetBuilder(url: URL):
    /**
      * Perform a GET request with no headers
      */
    def perform(): Future[Response] =
      GetRequest(url).perform()

    /**
      * Add an header to a GET request.
      * GET requests have no body.
      */
    infix def `with` (hm: HeaderParams): Future[Response] =
      GetRequest(url, hm.pairs).perform()

  /**
   * When we use .GET, just assign that to a Future[Response] and automatically run perform()
   */
  given Conversion[GetBuilder, Future[Response]] =
    (g: GetBuilder) => g.perform()


  /* 
   POST builder
   */
  case class HeaderParams(pairs: List[(String, String)])
  case class BodyParams(json: JsonValue)

  case class PostBuilder(url: URL):
    /* 
     POST with body == ... and headers == ...
     */
    infix def `with` (bm: BodyParams): BodyContent =
      BodyContent(url, bm.json)

    /* 
     POST with headers == ... and body == ...
     */
    infix def `with` (hm: HeaderParams): HeaderContent =
      HeaderContent(url, hm.pairs)

  /* 
   Allow the use of `and` keyword
   */
  case class BodyContent(url: URL, json: JsonValue):
    infix def and (hm: HeaderParams): Future[Response] =
      PostRequest(url, json.toString, hm.pairs).perform()

  case class HeaderContent(url: URL, headerParams: List[(String, String)]):
    infix def and (bm: BodyParams): Future[Response] =
      PostRequest(url, bm.json.toString, headerParams).perform()

  /* 
   Allow the use of `==` keyword
   */
  object headers:
    def == (pairs: (String, String)*): HeaderParams =
      HeaderParams(pairs.toList)

  object body:
    def == (json: JsonValue): BodyParams =
      BodyParams(json)

  // Expose GET/POST on UrlBuilder
  extension (b: UrlBuilder)
    inline def GET: GetBuilder = GetBuilder(b.build)
    inline def POST: PostBuilder = PostBuilder(b.build)

end HttpRequestDSL

@main def exercise2() =

  // DO NOT touch this
  import JsonDSL.{given, *}
  import HttpRequestDSL.{given, *}
  import AsyncContext.{given, *}

  // Exercise 2, Uncomment below
  val getRequest1: Future[Response] =
    (https `://` "www.usi.ch" / "en" / "university").GET

  /*
   - https ... -> UrlBuilder
	 - .GET -> GetBuilder(url)
   - Conversion calls perform() -> Future[Response]
   */
  val getRequest2 =
    (https `://` "reqres.in" / "api" / "user" / "1").GET `with` headers == (  // Scala parses it as ( ( ("reqres.in" / "api") / "user" ) / "1" )
      "x-api-key" -> "reqres-free-v1"
    )

  val getRequest3 = (http `://` "usi.cch").GET

  val postRequest1 =
    (https `://` "reqres.in" / "api" / "users").POST `with` body == jobj(
      "name" `:` "morpheus",
      "job" `:` "leader"
    ) and headers == (
      "x-api-key" -> "reqres-free-v1"
    )

  val postRequest2 =
    (https `://` "reqres.in" / "api" / "register").POST `with` body == jobj(
      "email" `:` "agent.smith@reqres.in",
      "password" `:` "OguhGnivaew"
    ) and headers == (
      "x-api-key" -> "reqres-free-v1"
    )

  val postRequest3 =
    (https `://` "reqres.in" / "api" / "login").POST `with` headers == (
      "x-api-key" -> "reqres-free-v1"
    )  and body == jobj(
      "email" `:` "morpheus@nebuchadnezzar"
    )

  // Do not touch this, just uncomment below.
  executeInSequence(
    getRequest1,
    getRequest2,
    getRequest3,
    postRequest1,
    postRequest2,
    postRequest3
  )
end exercise2

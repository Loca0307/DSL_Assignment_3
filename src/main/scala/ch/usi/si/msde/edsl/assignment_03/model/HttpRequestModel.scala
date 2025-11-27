package ch.usi.si.msde.edsl.assignment_03.model

import scala.concurrent.Future
import scala.util.Success
import org.apache.pekko.http.scaladsl.model.HttpResponse
import org.apache.pekko.http.scaladsl.model.HttpRequest
import org.apache.pekko.http.scaladsl.model.HttpEntity
import org.apache.pekko.http.scaladsl.model.HttpMethods
import org.apache.pekko.http.scaladsl.Http
import org.apache.pekko.http.scaladsl.model.ContentTypes
import org.apache.pekko.http.scaladsl.unmarshalling.*
import org.apache.pekko.http.scaladsl.common.EntityStreamingSupport
import org.apache.pekko.http.scaladsl.common.JsonEntityStreamingSupport
import spray.json.JsValue
import spray.json.*
import DefaultJsonProtocol.*
import scala.util.Try
import org.apache.pekko.http.scaladsl.model.HttpHeader
import org.apache.pekko.http.scaladsl.model.HttpHeader.ParsingResult

/** A simple model for Http requests
  */
object HttpRequestModel:

  // an given value that allows async computations on futures.
  import AsyncContext.ec

  /** A trait modeling a request on a URL.
    */
  trait Request:
    val url: URL

  /** A http get request.
    *
    * @param url
    *   a url.
    * @param headers
    *   a list of headers as pairs of strings.
    */
  case class GetRequest(url: URL, headers: List[(String, String)] = List())
      extends Request:
    /** Performs the Get requests.
      *
      * The given instance is available if you do not touch the imports.
      *
      * @param system
      * @return
      *   a future of a response.
      */
    def perform()(using
        system: org.apache.pekko.actor.ClassicActorSystemProvider
    ): Future[Response] =
      Future:
        headers.map: pair =>
          HttpHeader.parse(pair._1, pair._2) match
            case ParsingResult.Ok(h, _) => h
            case _ =>
              throw IllegalArgumentException(s"Invalid Header pair $pair")
      .flatMap: parsedHeaders =>
        Http()
          .singleRequest(
            HttpRequest(uri = url.toString, headers = parsedHeaders)
          )
          .flatMap: pekkoResponse =>
            val futureBodyString = Unmarshal(pekkoResponse.entity).to[String]
            futureBodyString.map: bodyString =>
              Response(pekkoResponse, bodyString)
  end GetRequest

  case class PostRequest(
      url: URL,
      rawJsonEntity: String,
      headers: List[(String, String)] = List()
  ) extends Request:
    /** Performs a Post request.
      *
      * The given instance is available if you do not touch the imports.
      *
      * @param system
      * @return
      *   a future of a response.
      */
    def perform()(using
        system: org.apache.pekko.actor.ClassicActorSystemProvider
    ): Future[Response] =
      Future:
        headers.map: pair =>
          HttpHeader.parse(pair._1, pair._2) match
            case ParsingResult.Ok(h, _) => h
            case _ =>
              throw IllegalArgumentException(s"Invalid Header pair $pair")
      .flatMap: parsedHeaders =>
        Http()
          .singleRequest(
            HttpRequest(
              method = HttpMethods.POST,
              uri = url.toString,
              entity =
                HttpEntity(ContentTypes.`application/json`, rawJsonEntity),
              headers = parsedHeaders
            )
          )
          .flatMap: pekkoResponse =>
            val futureBodyString = Unmarshal(pekkoResponse.entity).to[String]
            futureBodyString.map: bodyString =>
              // println(bodyString)
              Response(pekkoResponse, bodyString)

  end PostRequest

  /** A view of a http response, including the status code, the content types,
    * and the headers.
    *
    * @param pekkoResponse
    *   the complete http response.
    */
  class Response(private val pekkoResponse: HttpResponse, stringBody: String):
    lazy val statusCode = pekkoResponse.status.intValue()
    lazy val headers: Seq[(String, String)] = pekkoResponse.headers.map {
      header => header.name() -> header.value
    }
    lazy val contentType = pekkoResponse.entity.contentType.mediaType.toString()
    lazy val jsonBody: Try[JsValue] = Try { stringBody.parseJson }

    override def toString =
      s"Response with code ${statusCode} and content type ${contentType}"
  end Response

  /** Executes some futures in sequence, and then terminates.
    *
    * @param seq
    *   a sequence of futures.
    */
  def executeInSequence[T](seq: Future[T]*): Unit =
    val transformedSeq = seq map { _.transform(t => Success(t)) }
    Future.sequence(transformedSeq) map { list =>
      list.foreach(println)
    } foreach { _ =>
      AsyncContext.system.terminate()
    }
  end executeInSequence

end HttpRequestModel

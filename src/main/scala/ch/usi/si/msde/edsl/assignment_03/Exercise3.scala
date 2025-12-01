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
import ch.usi.si.msde.edsl.assignment_03.HttpRequestDSL.GetBuilder
import ch.usi.si.msde.edsl.assignment_03.HttpRequestDSL.PostBuilder

trait RequestAssertionDSL extends AssertionExecutor:

  // Do not touch this import
  import model.AsyncContext.*

  // Write your DSL below

  // keep the last eventual to handle parsing grouping variations
  private var lastEventualRequest: Option[EventualRequest] = None
  private var lastDescription: Option[AssertionDescription] = None

  // Capture a request without evaluating it.
  // it produces a Future[Response] when called
  case class EventualRequest(eval: () => Future[Response]):
    // If parsing bound `should` to the rhs instead of
    // the whole pending assertion, we consult the lastDescription stored
    // by DescriptionBuilder.should and register the assertion
    infix def should(token: respond.type): RespondBuilder =
      val desc = lastDescription.getOrElse(throw IllegalArgumentException("No description found"))
      RespondBuilder(desc, this)

    infix def should(token: fail.type): Unit =
      val desc = lastDescription.getOrElse(throw IllegalArgumentException("No description found"))
      namedAssertions = namedAssertions :+ AssertionWithDescription(desc, RequestWillFailAssertion(this.eval))

  // TODO: remove .asInstanceOf
  def eventually (req: => Any): EventualRequest =
    val er = EventualRequest(() =>
      req match
        case f: Future[_] => f.asInstanceOf[Future[Response]]
        case gb: GetBuilder       => gb.perform()
        case pb: PostBuilder      =>
          throw IllegalArgumentException("POST must be completed with body/headers before asserting")
        case other =>
          try other.asInstanceOf[Future[Response]]
          catch
            case _: Throwable => throw IllegalArgumentException(s"Unsupported request expression: $other")
    )
    lastEventualRequest = Some(er)
    er

  // Description builder
  case class DescriptionBuilder(subject: String, expected: String):
    infix def += (ev: Any): PendingAssertion =
      val desc = AssertionDescription(subject, expected)
      val eventual = ev match
        case er: EventualRequest => er
        case _: Unit => lastEventualRequest.getOrElse(throw IllegalArgumentException("No eventual request found"))
        case other => throw IllegalArgumentException(s"Unsupported RHS for +=: $other")
      PendingAssertion(desc, eventual)

  /* 
   Entry point ("a get on user 1 with api key")

   "a get on user 1 with api key" should "respond with 200 ok"
   is parsed as
   "a get on user 1 with api key".should("respond with 200 ok")
   */ 
  extension (subject: String)
    infix def should (expected: String): DescriptionBuilder =
      val d = AssertionDescription(subject, expected)
      // store as last description
      lastDescription = Some(d)
      DescriptionBuilder(subject, expected)

  // Pending assertion after +=. It extends EventualRequest
  // Pending assertion after +=: this class exposes member `should` so chains
  // of `should` respond correctly.
  class PendingAssertion(val desc: AssertionDescription, val ev: EventualRequest):
    def should (token: respond.type): RespondBuilder = RespondBuilder(desc, ev)
    def should (token: fail.type): Unit =
      namedAssertions = namedAssertions :+ AssertionWithDescription(desc, RequestWillFailAssertion(ev.eval))

  // Response predicate helpers
  object statusCode:
    def apply (n: Int): ResponsePredicate = ResponseHasStatusCodeEqualsToPredicate(n)

  object contentType:
    def apply (s: String): ResponsePredicate = ResponseHasContentTypeEqualsToPredicate(s)

  object responseBody:
    def apply (obj: JsonObject): ResponsePredicate = ResponseContainsJson(obj)

  // Predicate combinators
  extension (a: ResponsePredicate)
    infix def | (b: ResponsePredicate): ResponsePredicate = OrPredicate(a, b)
    infix def & (b: ResponsePredicate): ResponsePredicate = AndPredicate(a, b)

  // markers
  object respond
  object fail

  // member methods on PendingAssertion handle should/fail

  case class RespondBuilder(desc: AssertionDescription, ev: EventualRequest):
    infix def `with` (p: ResponsePredicate): Unit =
      val assertion = RequestSucceedsWithResponsePredicateAssertion(ev.eval, p)
      namedAssertions = namedAssertions :+ AssertionWithDescription(desc, assertion)

end RequestAssertionDSL

class Exercise3_Example extends RequestAssertionDSL:

  // Do not touch this
  import JsonDSL.{given, *}
  import HttpRequestDSL.{given, *}
  import model.AsyncContext.{given, *}

  // Exercise 3: respond/fail assertions
  // uncomment below

  "a get on user 1 with api key" should "respond with 200 ok" += eventually (
    (https `://` "reqres.in" / "api" / "user" / "1").GET `with` headers == (
      "x-api-key" -> "reqres-free-v1"
    )
  ) should respond `with` statusCode(200)

  "a get on user 1 without api key" should "respond with 200 or 401" += eventually (
    (https `://` "reqres.in" / "api" / "user" / "1").GET
  ) should respond `with` statusCode(200) | statusCode(401)

  "a get on user 3 with api key" should "respond with some json" += eventually (
    (https `://` "reqres.in" / "api" / "user" / "3").GET `with` headers == (
      "x-api-key" -> "reqres-free-v1"
    )
  ) should respond `with` contentType("application/json")

  "a get on a non-existing user" should "respond with 404" += eventually (
    (https `://` "reqres.in" / "api" / "user" / "-3721").GET `with` headers == (
      "x-api-key" -> "reqres-free-v1"
    )
  ) should respond `with` statusCode(404) & contentType("application/json")

  "a GET on a non-existing domain" should "fail" += eventually (
    (https `://` "www.usi.chh").GET
   ) should fail

  "a login without password" should "respond with contentType json" += eventually (
    (https `://` "reqres.in" / "api" / "login").POST `with` headers == (
      "x-api-key" -> "reqres-free-v1"
    ) and body == jobj(
      "email" `:` "morpheus@nebuchadnezzar"
    )
  ) should respond `with` contentType("application/json")

  "a login without password" should "respond with a json body containing an error" += eventually (
    (https `://` "reqres.in" / "api" / "login").POST `with` headers == (
      "x-api-key" -> "reqres-free-v1"
    ) and body == jobj(
      "email" `:` "morpheus@nebuchadnezzar"
    )
  ) should respond `with` responseBody(jobj(
    "error" `:` "Missing password"
  )) & statusCode(400)

  // do ** NOT ** remove this
  run()
end Exercise3_Example

@main def exercise3 = Exercise3_Example()

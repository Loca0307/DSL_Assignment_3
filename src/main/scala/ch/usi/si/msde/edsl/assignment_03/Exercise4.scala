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

trait RequestAssertionTwoDSL extends AssertionExecutor:

  // Do not touch this import
  import model.AsyncContext.*

  // Write your DSL below

  private var lastEventualRequest: Option[EventualRequest] = None
  private var lastDescription: Option[AssertionDescription] = None

  case class EventualRequest(eval: () => Future[Response]):
    infix def should(token: respond.type): RespondBuilder =
      val desc = lastDescription.getOrElse(throw IllegalArgumentException("No description found"))
      RespondBuilder(desc, this)

    infix def should(token: fail.type): Unit =
      val desc = lastDescription.getOrElse(throw IllegalArgumentException("No description found"))
      namedAssertions = namedAssertions :+ AssertionWithDescription(desc, RequestWillFailAssertion(this.eval))

  def eventually (req: => Any): EventualRequest =   // with the => Any, we're saying that req is evaluated calling eval()
    val er = EventualRequest(() =>
      req match
        case gb: GetBuilder  => gb.perform()
        case pb: PostBuilder =>
          // POST must be fully built (headers/body) before asserting.
          throw IllegalArgumentException("POST must be completed with body/headers before asserting")
        case f: Future[_] =>
          f.asInstanceOf[Future[Response]]
    )
    lastEventualRequest = Some(er)
    er

  case class DescriptionBuilder(subject: String, expected: String):
    infix def += (ev: Unit): PendingAssertion =
      // Create the semantic description from the two strings
      val desc = AssertionDescription(subject, expected)

      val eventual = 
        lastEventualRequest.getOrElse(
          throw IllegalArgumentException("No eventual request found")
        )
      
      // We don't yet know if it's `respond` or `fail`, so we return a PendingAssertion
      PendingAssertion(desc, eventual)

  extension (subject: String)
    infix def should (expected: String): DescriptionBuilder =
      val d = AssertionDescription(subject, expected)
      // store as last description
      lastDescription = Some(d)
      DescriptionBuilder(subject, expected)

  class PendingAssertion(val desc: AssertionDescription, val ev: EventualRequest):
    def should (token: respond.type): RespondBuilder = 
      RespondBuilder(desc, ev)
      
    def should (token: fail.type): Unit =
      namedAssertions = namedAssertions :+ AssertionWithDescription(desc, RequestWillFailAssertion(ev.eval))

  object statusCode:
    def apply (n: Int): ResponsePredicate = ResponseHasStatusCodeEqualsToPredicate(n)

  object contentType:
    def apply (s: String): ResponsePredicate = ResponseHasContentTypeEqualsToPredicate(s)

  object responseBody:
    def apply (obj: JsonObject): ResponsePredicate = ResponseContainsJson(obj)

  object respond
  object fail

  case class RespondBuilder(desc: AssertionDescription, ev: EventualRequest):
    infix def `with`(p: ResponsePredicate): Unit =
      val assertion = RequestSucceedsWithResponsePredicateAssertion(ev.eval, p)
      namedAssertions = namedAssertions :+ AssertionWithDescription(desc, assertion)

  /* 
    NEW BOOLEAN LOGIC.
    
    This wouldn't work:
      extension (a: ResponsePredicate)
        infix def or (b: ResponsePredicate): ResponsePredicate = OrPredicate(a, b)
        infix def and (b: ResponsePredicate): ResponsePredicate = AndPredicate(a, b)
   
    For example:
      statusCode(404) or statusCode(200) and contentType("application/json")
    would be parsed as:
      (statusCode(404).or(statusCode(200))).and(contentType("application/json"))
    because Scala's method calls are left-associative.

    Normal boolean logic would interpret it as:
      (statusCode(404).or(statusCode(200)).and(contentType("application/json"))
    because `and` has higher precedence than `or`.

    We want something like:
      OrPredicate(
        ResponseHasStatusCodeEqualsToPredicate(404),
        AndPredicate(
          ResponseHasStatusCodeEqualsToPredicate(401),
          ResponseHasContentTypeEqualsToPredicate("json")
        )
      )
    and not
      AndPredicate(
        OrPredicate(
          ResponseHasStatusCodeEqualsToPredicate(404),
          ResponseHasStatusCodeEqualsToPredicate(401),
        ),
        ResponseHasContentTypeEqualsToPredicate("json")
    )

    Idea:
    Everything inside the parentheses of
      respond `with` (boolean expression)
    is evaluated BEFORE `with` is called.
    So the ResponsePredicate is rewritten in method calls.
    Then, the extension methods invoke BooleanLogic which
    fixes the operator precedence and build a single ResponsePredicate tree.
    The resulting ResponsePredicate is what gets passed as the argument `p`
    to the `with`(p) method.
   */
        
  /**
    * These are used after `with` to combine predicates:
    *   statusCode(404) or statusCode(401) and contentType("application/json")
    * We delegate the logic to BooleanLogic to create the ResponsePredicate using a tree
    */
  extension (a: ResponsePredicate)
    infix def and(b: ResponsePredicate): ResponsePredicate = BooleanLogic.and(a, b)
    infix def or(b: ResponsePredicate): ResponsePredicate = BooleanLogic.or(a, b)

  /**
    * Helper for boolean logic.
    * It builds predicate trees with correct precedence.
    */
  object BooleanLogic:
    /** 
      * Simple case.
      * Wrap the whole existing expression on the left.
      */
    def or(a: ResponsePredicate, b: ResponsePredicate): ResponsePredicate =
      OrPredicate(a, b)

    /** 
      * AND must respect its precedence
      * 
      *   E and x =
      *     if E is OrPredicate(a, b)
      *       OrPredicate(a, b and x)   // push AND into the right branch
      *     else
      *       AndPredicate(E, x)        // chain with AND at this level
      */
    def and(a: ResponsePredicate, b: ResponsePredicate): ResponsePredicate =
      a match
        case OrPredicate(left, right) =>
          // Recursive call ensures that if the right branch is also an OR,
          // the AND continues propagating correctly into the deepest right side.
          OrPredicate(left, and(right, b))
        case _ =>
          // no top-level OR: simple AND composition at this level
          AndPredicate(a, b)

  end BooleanLogic

end RequestAssertionTwoDSL

class Exercise4_Example extends RequestAssertionTwoDSL:

  import JsonDSL.{given, *}
  import HttpRequestDSL.{given, *}
  import model.AsyncContext.{given, *}

  "a get on a non-existing user" should "respond with 401" += eventually (
    (https `://` "reqres.in" / "api" / "user" / "-3721").GET `with` headers == (
      "x-api-key" -> "reqres-free-v1"
    )
  ) should respond `with` (statusCode(999) or statusCode(401) and contentType("application/json"))

  "a login without password" should "respond with a json body containing an error" += eventually (
    (https `://` "reqres.in" / "api" / "login").POST `with` headers == (
      "x-api-key" -> "reqres-free-v1"
    ) and body == jobj(
      "email" `:` "morpheus@nebuchadnezzar"
    )
  ) should respond `with` (responseBody(jobj(
    "error" `:` "Missing password",
    "message" `:` "Create your API key at https://app.reqres.in to access the ReqRes API.",
    "signup_url" `:` "https://app.reqres.in"
  )) and statusCode(401))

  "a get on user 1 with api key" should "respond with 200 ok" += eventually (
    (https `://` "reqres.in" / "api" / "user" / "1").GET `with` headers == (
      "x-api-key" -> "reqres-free-v1"
    )
  ) should respond `with` statusCode(200)

  "a get on user 1 without api key" should "respond with 200 or 401" += eventually (
    (https `://` "reqres.in" / "api" / "user" / "1").GET
  ) should respond `with` (statusCode(200) or statusCode(401))

  "a get on user 3 with api key" should "respond with some json" += eventually (
    (https `://` "reqres.in" / "api" / "user" / "3").GET `with` headers == (
      "x-api-key" -> "reqres-free-v1"
    )
  ) should respond `with` contentType("application/json")

  "a get on a non-existing user" should "respond with 401" += eventually (
    (https `://` "reqres.in" / "api" / "user" / "-3721").GET `with` headers == (
      "x-api-key" -> "reqres-free-v1"
    )
  ) should respond `with` (statusCode(401) and contentType("application/json") or statusCode(999))

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
  ) should respond `with` (responseBody(jobj(
    "error" `:` "Missing password"
  )) and (statusCode(400) or statusCode(999)))

  run()
end Exercise4_Example

@main def exercise4 = Exercise4_Example()

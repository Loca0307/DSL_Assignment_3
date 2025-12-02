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
    infix def += (ev: Any): PendingAssertion =
      val desc = AssertionDescription(subject, expected)
      val eventual = ev match
        case er: EventualRequest => er
        case _ =>
          lastEventualRequest.getOrElse(
            throw IllegalArgumentException("No eventual request found")
          )
      PendingAssertion(desc, eventual)

  extension (subject: String)
    infix def should (expected: String): DescriptionBuilder =
      val d = AssertionDescription(subject, expected)
      // store as last description
      lastDescription = Some(d)
      DescriptionBuilder(subject, expected)

  class PendingAssertion(val desc: AssertionDescription, val ev: EventualRequest):
    def should (token: respond.type): RespondBuilder = RespondBuilder(desc, ev)
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

  /* 
    BOOLEAN LOGIC.
    
    This wouldn't work:
      extension (a: ResponsePredicate)
        infix def or (b: ResponsePredicate): ResponsePredicate = OrPredicate(a, b)
        infix def and (b: ResponsePredicate): ResponsePredicate = AndPredicate(a, b)
   
    For example:
      statusCode(404) or statusCode(200) and contentType("application/json")
    would be parsed as:
      (statusCode(404).or(statusCode(200))).and(contentType("application/json"))
    but normal boolean logic would interpret it as:
      (statusCode(404).or(statusCode(200)).and(contentType("application/json"))
    because `and` has higher precedence than `or`
   */

  /**
    * Holds:
    *  - the description and eventual request (`desc`, `ev`)
    *  - the current accumulated predicate (`current`)
    * 
    * This is what `respond with ...` returns, so we can keep chaining
    * `.or(...)` / `.and(...)` after `with`.
    */
  case class PredicateChainBuilder(
    desc: AssertionDescription,
    ev: EventualRequest,
    var current: ResponsePredicate
  )

  /**
    * It holds:
    *  - the EventualRequest which knows how to execute the HTTP call
    *  - the PredicateChainBuilder whose `current` field will be
    *    mutated by next `.or` or `.and` calls
    *
    * At run-time, we read `chain.current` to get the FINAL predicate, after
    * all boolean chaining is done
    */
  case class PredicateAssertion(
    ev: EventualRequest,
    chain: PredicateChainBuilder
  ) extends ExecutableAssertion:
    def run(description: AssertionDescription): Future[AssertionResult] =
      // Get the final predicate at execution time
      val finalPredicate = chain.current
      
      // Delegate to the existing assertion implementation from the model
      RequestSucceedsWithResponsePredicateAssertion(ev.eval, finalPredicate)
        .run(description)
        
  /**
    * These are used when predicates are combined directly, e.g.:
    * statusCode(404) or statusCode(401) and contentType("application/json")
    */
  extension (a: ResponsePredicate)
    infix def and(b: ResponsePredicate): ResponsePredicate = BooleanLogic.and(a, b)
    infix def or(b: ResponsePredicate): ResponsePredicate = BooleanLogic.or(a, b)

  /**
    * Centralized boolean logic for ResponsePredicate
    */
  object BooleanLogic:
    /** 
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
          // push the new AND into the right-hand side recursively
          OrPredicate(left, and(right, b))
        case _ =>
          // no top-level OR: simple AND on this level
          AndPredicate(a, b)

  end BooleanLogic

  /**
    * RespondBuilder represents:
    * ... should respond
    *
    * From here, the user will write:
    * ... should respond `with` statusCode(200) or statusCode(401) and ...
    *
    * The `with` method:
    *  - starts the chain with the first predicate
    *  - registers a PredicateAssertion
    *  - returns a PredicateChainBuilder so `.or` or `.and` can mutate the predicate
    */
  case class RespondBuilder(desc: AssertionDescription, ev: EventualRequest):
    /**
      * Starts a predicate chain after `respond`
      */
    infix def `with`(p: ResponsePredicate): PredicateChainBuilder =
      // start a new chain
      val chain = PredicateChainBuilder(desc, ev, p)
      // register a single assertion that will use chain.current at runtime
      val assertion = PredicateAssertion(ev, chain)
      namedAssertions =
        namedAssertions :+ AssertionWithDescription(desc, assertion)
      // return builder so we can chain .or or .and
      chain

  /**
    * Boolean operations on PredicateChainBuilder.
    * These methods:
    *  - update the builder's `current` predicate using BooleanLogic
    *  - return the same builder so the chain can continue
    * 
    * Example:
    * ... should respond `with` statusCode(200) or statusCode(401) and contentType("json")
    * works without parentheses
    */
  extension (chain: PredicateChainBuilder)
    infix def and(b: ResponsePredicate): PredicateChainBuilder =
      chain.current = BooleanLogic.and(chain.current, b)
      chain

    infix def or(b: ResponsePredicate): PredicateChainBuilder =
      chain.current = BooleanLogic.or(chain.current, b)
      chain

end RequestAssertionTwoDSL

class Exercise4_Example extends RequestAssertionTwoDSL:

  import JsonDSL.{given, *}
  import HttpRequestDSL.{given, *}
  import model.AsyncContext.{given, *}

  "a get on a non-existing user" should "respond with 404" += eventually (
    (https `://` "reqres.in" / "api" / "user" / "-3721").GET `with` headers == (
      "x-api-key" -> "reqres-free-v1"
    )
  ) should respond `with` (statusCode(404) or statusCode(401)) and contentType("application/json")

  "a login without password" should "respond with a json body containing an error" += eventually (
    (https `://` "reqres.in" / "api" / "login").POST `with` headers == (
      "x-api-key" -> "reqres-free-v1"
    ) and body == jobj(
      "email" `:` "morpheus@nebuchadnezzar"
    )
  ) should respond `with` responseBody(jobj(
    "error" `:` "Missing password",
    "message" `:` "Create your API key at https://app.reqres.in to access the ReqRes API.",
    "signup_url" `:` "https://app.reqres.in"
  )) and statusCode(401)

  "a get on user 1 with api key" should "respond with 200 ok" += eventually (
    (https `://` "reqres.in" / "api" / "user" / "1").GET `with` headers == (
      "x-api-key" -> "reqres-free-v1"
    )
  ) should respond `with` statusCode(200)

  "a get on user 1 without api key" should "respond with 200 or 401" += eventually (
    (https `://` "reqres.in" / "api" / "user" / "1").GET
  ) should respond `with` statusCode(200) or statusCode(401)

  "a get on user 3 with api key" should "respond with some json" += eventually (
    (https `://` "reqres.in" / "api" / "user" / "3").GET `with` headers == (
      "x-api-key" -> "reqres-free-v1"
    )
  ) should respond `with` contentType("application/json")

  "a get on a non-existing user" should "respond with 401" += eventually (
    (https `://` "reqres.in" / "api" / "user" / "-3721").GET `with` headers == (
      "x-api-key" -> "reqres-free-v1"
    )
  ) should respond `with` statusCode(401) and contentType("application/json") or statusCode(999)

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
  )) and (statusCode(400) or statusCode(999))

  run()
end Exercise4_Example

@main def exercise4 = Exercise4_Example()

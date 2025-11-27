package ch.usi.si.msde.edsl.assignment_03.model

import scala.concurrent.Future

/** This trait is used to execute a set of assertions and exit the system.
  *
  * Insert the assertions to execute in the namedAssertions var, and then call
  * run().
  */
trait AssertionExecutor:

  import AssertionModel.{given, *}

  protected var namedAssertions: List[AssertionWithDescription] = List()

  import AsyncContext.{given, *}

  /** Checks the assertions, prints the result, and terminates.
    */
  def run() =
    val assertionFutures = namedAssertions.map:
      _.run()
    Future
      .sequence(assertionFutures)
      .map: results =>
        results.foreach(println)
        system.terminate()
  end run

end AssertionExecutor

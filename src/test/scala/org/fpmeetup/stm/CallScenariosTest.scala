package org.fpmeetup.stm

import cats._
import cats.data._
import cats.implicits._
import org.scalatest.FunSuite

class CallScenariosTest extends FunSuite {
  test("Call in script goes to agent") {
    runRightScenario(
      TargetAgent("a1") -> Presenting("a1"),
      Accept -> Processing("a1"),
      CallEnded -> PostProcessing("a1"),
      PostProcessingEnded -> Ended
    )
  }

  test("Call in queue goes to agent") {
    runRightScenario(
      Enqueue("q1") -> InQueue("q1"),
      TargetAgent("a1") -> Presenting("a1"),
      Accept -> Processing("a1"),
      CallEnded -> PostProcessing("a1"),
      PostProcessingEnded -> Ended
    )
  }

  test("Call needs to get back to queue if agent rejects") {
    runRightScenario(
      Enqueue("q1") -> InQueue("q1"),
      TargetAgent("a1") -> Presenting("a1"),
      Reject -> InScript,
      Enqueue("q1") -> InQueue("q1"),
    )
  }

  test("Call cannot end while in presenting") {
    runScenario(
      Enqueue("q1") -> Right(InQueue("q1")),
      TargetAgent("a1") -> Right(Presenting("a1")),
      CallEnded -> Left(InvalidCallModelTransition(Presenting("a1"), CallEnded))
    )
  }

  private def runRightScenario(opsAndStates: (CallOp, CallState)*): Unit = {
    val opsAndRightStates = opsAndStates.map({ case (op, state) => (op, state.asRight[Any]) })
    runScenario(opsAndRightStates: _*)
  }

  test("An agent cannot reject after accepting a call") {
    assertThrows[RuntimeException] {
      runScenario( // @formatter:off
        Enqueue("q1")     -> InQueue("q1"),
        TargetAgent("a1") -> Presenting("a1"),
        Accept            -> Processing("a1"),
        Reject            -> Ended // this result won't actually be here
      ) // @formatter:off

      // but where does it throw? does it really throw at the last step?
    }
  }

  private def runScenario(opsAndErrorOrStates: (CallOp, Either[Any, CallState])*) {
    val (ops, errorOrStates) = opsAndErrorOrStates.unzip

    // type inference issues with `Right(CallModel.init)`
    val actualStates = ops.scanLeft(CallModel.init.asRight[Any])((either, op) => either.flatMap(st => CallModel.fun(st, op)))
    assert(actualStates.tail == errorOrStates)
  }
}

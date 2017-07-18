package org.fpmeetup.stm

import cats._
import cats.data._
import cats.implicits._
import org.scalatest.FunSuite

class CallScenariosTest extends FunSuite {
  test("Call in script goes to agent") {
    runScenario(
      TargetAgent("a1")   -> Right(Presenting("a1")),
      Accept              -> Right(Processing("a1")),
      CallEnded           -> Right(PostProcessing("a1")),
      PostProcessingEnded -> Right(Ended)
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
      CallEnded -> Left(InvalidCallTransition(Presenting("a1"), CallEnded))
    )
  }

  test("An agent cannot reject after accepting a call") {
    runScenario(
        Enqueue("q1")     -> Right(InQueue("q1")),
        TargetAgent("a1") -> Right(Presenting("a1")),
        Accept            -> Right(Processing("a1")),
        Reject            -> Left(InvalidCallTransition(Processing("a1"), Reject))
      )
  }

  private def runRightScenario(opsAndStates: (CallOp, CallState)*): Unit = {
    val opsAndRightStates = opsAndStates.map({ case (op, state) => (op, state.asRight[CallError]) })
    runScenario(opsAndRightStates: _*)
  }

  private def runScenario(opsAndErrorOrStates: (CallOp, CallErrorOr[CallState])*) {
    val (ops, callErrorOrStates) = opsAndErrorOrStates.unzip

    val z: CallErrorOr[CallState] = Right(CallModel.init)
    val actualStates = ops.scanLeft(z)((either, op) => either.flatMap(st => CallModel.fun(st, op)))
    assert(actualStates.tail == callErrorOrStates)
  }
}

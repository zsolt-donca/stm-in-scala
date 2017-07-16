package org.fpmeetup.stm

import org.scalatest.FunSuite

class CallScenariosTest extends FunSuite {
  test("Call in script goes to agent") {
    runScenario( // @formatter:off
      TargetAgent("a1")   -> Presenting("a1"),
      Accept              -> Processing("a1"),
      CallEnded           -> PostProcessing("a1"),
      PostProcessingEnded -> Ended
    ) // @formatter:on
  }

  test("Call in queue goes to agent") {
    runScenario( // @formatter:off
      Enqueue("q1")       -> InQueue("q1"),
      TargetAgent("a1")   -> Presenting("a1"),
      Accept              -> Processing("a1"),
      CallEnded           -> PostProcessing("a1"),
      PostProcessingEnded -> Ended
    ) // @formatter:on
  }

  test("Call needs to get back to queue if agent rejects") {
    runScenario( // @formatter:off
      Enqueue("q1")     -> InQueue("q1"),
      TargetAgent("a1") -> Presenting("a1"),
      Reject            -> InScript,
      Enqueue("q1")     -> InQueue("q1"),
    ) // @formatter:on
  }

  test("Call cannot end while in presenting") {
    assertThrows[RuntimeException] {
      runScenario( // @formatter:off
        Enqueue("q1")     -> InQueue("q1"),
        TargetAgent("a1") -> Presenting("a1"),
        CallEnded         -> Ended // this result won't actually be here
      ) // @formatter:off

      // but where does it throw? does it really throw at the last step?
    }
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

  private def runScenario(opsAndStates: (CallOp, CallState)*) {
    val (ops, states) = opsAndStates.unzip

    val actualStates = ops.scanLeft(CallModel.init)(CallModel.fun)
    assert(actualStates.tail == states)
  }
}

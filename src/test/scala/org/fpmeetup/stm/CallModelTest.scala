package org.fpmeetup.stm

import org.scalatest.WordSpec

class CallModelTest extends WordSpec {
  "While In Script" when {
    "enqueuing" should {
      "result in being In Queue" in {
        assert(CallModel.fun(InScript, Enqueue("q1")) == Right(InQueue("q1")))
      }
    }

    "targeting an agent" should {
      "result in beging Presenting for that agent" in {
        assert(CallModel.fun(InScript, TargetAgent("a1")) == Right(Presenting("a1")))
      }
    }

    for (op <- Seq(Dequeue, Accept, Reject, CallEnded, PostProcessingEnded)) {
      s"doing $op" should {
        "result in an error" in {
          assert(CallModel.fun(InScript, op) == Left(InvalidCallModelTransition(InScript, op)))
        }
      }
    }
  }

  "While In Queue" when {
    "dequeuing" should {
      "result in being In Script" in {
        assert(CallModel.fun(InQueue("q1"), Dequeue) == Right(InScript))
      }
    }
  }
}

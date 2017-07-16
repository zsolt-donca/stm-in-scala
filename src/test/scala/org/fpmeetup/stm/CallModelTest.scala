package org.fpmeetup.stm

import org.scalatest.WordSpec

class CallModelTest extends WordSpec {
  "While In Script" when {
    "enqueuing" should {
      "result in being In Queue" in {
        assert(CallModel.fun(InScript, Enqueue("q1")) == InQueue("q1"))
      }
    }

    "targeting an agent" should {
      "result in beging Presenting for that agent" in {
        assert(CallModel.fun(InScript, TargetAgent("a1")) == Presenting("a1"))
      }
    }

    for (invalidOp <- Seq(Dequeue, Accept, Reject, CallEnded, PostProcessingEnded)) {
      s"doing $invalidOp" should {
        "result in an error" in {
          assertThrows[RuntimeException] {
            CallModel.fun(InScript, invalidOp)
          }
          // what if I wanted to test where it's the right error message?
        }
      }
    }
  }

  "While In Queue" when {
    "dequeuing" should {
      "result in being In Script" in {
        assert(CallModel.fun(InQueue("q1"), Dequeue) == InScript)
      }
    }
  }
}

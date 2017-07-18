package org.fpmeetup.stm

import org.fpmeetup.stm.error.InvalidCallTransition
import org.scalatest.WordSpec
import org.fpmeetup.stm.call._

class CallTest extends WordSpec {
  "While In Script" when {
    "enqueuing" should {
      "result in being In Queue" in {
        assert(Call.fun(InScript, Enqueue("q1")) == Right(InQueue("q1")))
      }
    }

    "targeting an agent" should {
      "result in beging Presenting for that agent" in {
        assert(Call.fun(InScript, TargetAgent("a1")) == Right(Presenting("a1")))
      }
    }

    for (op <- Seq(Dequeue, Accept, Reject, CallEnded, PostProcessingEnded)) {
      s"doing $op" should {
        "result in an error" in {
          assert(Call.fun(InScript, op) == Left(InvalidCallTransition(InScript, op)))
        }
      }
    }
  }

  "While In Queue" when {
    "dequeuing" should {
      "result in being In Script" in {
        assert(Call.fun(InQueue("q1"), Dequeue) == Right(InScript))
      }
    }
  }
}

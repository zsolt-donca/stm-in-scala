package org.fpmeetup.stm

import cats._
import cats.data._
import cats.implicits._
import org.fpmeetup.stm.error.DuplicateCall
import org.scalatest.WordSpec
import org.fpmeetup.stm.callcenter._
import org.fpmeetup.stm.call._

class CallCenterTest extends WordSpec {
  "Creating a new call" when {
    "there are no calls in the call center" should {
      "create the first call and it is the Script state" in {
        assert(CallCenter.fun(Map.empty, CallCreated("c1")) == Map("c1" -> InScript).asRight)
      }
    }

    "there is another call in the call center" should {
      "create the call in the Script state and leave the other call as is" in {
        assert(CallCenter.fun(Map(c1 -> Processing(a1)), CallCreated(c2)) == Map(c1 -> Processing(a1), c2 -> InScript).asRight)
      }
    }

    "there is another call with the same id" should {
      "result in a relevant error" in {
        assert(CallCenter.fun(Map(c1 -> Processing(a1)), CallCreated(c1)) == DuplicateCall(c1, Processing(a1)).asLeft)
      }
    }
  }

  "Updating a call" when {
    "the call exists and accepts the transition" should {
      "update the call" in {
        val callCenterBefore = Map(c1 -> Processing(a1), c2 -> InScript)
        val callCenterAfter = Map(c1 -> PostProcessing(a1), c2 -> InScript)
        val op = CallUpdated(c1, CallEnded)
        assert(CallCenter.fun(callCenterBefore, op) == callCenterAfter.asRight)
      }
    }
  }

  val a1 = "agent1"
  val c1 = "call1"
  val c2 = "call2"
  val q1 = "queue1"
}

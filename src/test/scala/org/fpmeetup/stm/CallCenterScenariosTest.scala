package org.fpmeetup.stm

import org.fpmeetup.stm.FSMUtil._
import org.fpmeetup.stm.callcenter._
import org.fpmeetup.stm.call._
import org.scalatest.FunSuite

class CallCenterScenariosTest extends FunSuite {
  test("Two calls in parallel") {
    runRightScenario(CallCenter,
      CallCreated(c1) -> Map(c1 -> InScript),
      CallCreated(c2) -> Map(c1 -> InScript, c2 -> InScript),
      CallUpdated(c1, Enqueue(q1)) -> Map(c1 -> InQueue(q1), c2 -> InScript),
      CallUpdated(c1, TargetAgent(a1)) -> Map(c1 -> Presenting(a1), c2 -> InScript),
      CallUpdated(c1, Accept) -> Map(c1 -> Processing(a1), c2 -> InScript),
      CallUpdated(c1, CallEnded) -> Map(c1 -> PostProcessing(a1), c2 -> InScript),
      CallUpdated(c1, PostProcessingEnded) -> Map(c1 -> Ended, c2 -> InScript),
      CallDeleted(c1) -> Map(c2 -> InScript),
    )
  }

  val a1 = "agent1"
  val c1 = "call1"
  val c2 = "call2"
  val q1 = "queue1"
}

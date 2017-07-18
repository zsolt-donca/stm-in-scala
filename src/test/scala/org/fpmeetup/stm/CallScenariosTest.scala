package org.fpmeetup.stm

import cats._
import cats.data._
import cats.implicits._
import org.fpmeetup.stm.FSMUtil.{runRightScenario, runScenario}
import org.fpmeetup.stm.error.InvalidCallTransition
import org.scalatest.FunSuite
import org.fpmeetup.stm.call._

class CallScenariosTest extends FunSuite {
  test("Call in script goes to agent") {
    runScenario(Call,
      TargetAgent("a1")   -> Right(Presenting("a1")),
      Accept              -> Right(Processing("a1")),
      CallEnded           -> Right(PostProcessing("a1")),
      PostProcessingEnded -> Right(Ended)
    )
  }

  test("Call in queue goes to agent") {
    runRightScenario(Call,
      Enqueue("q1")       -> InQueue("q1"),
      TargetAgent("a1")   -> Presenting("a1"),
      Accept              -> Processing("a1"),
      CallEnded           -> PostProcessing("a1"),
      PostProcessingEnded -> Ended
    )
  }

  test("Call needs to get back to queue if agent rejects") {
    runRightScenario(Call,
      Enqueue("q1")     -> InQueue("q1"),
      TargetAgent("a1") -> Presenting("a1"),
      Reject            -> InScript,
      Enqueue("q1")     -> InQueue("q1"),
    )
  }

  test("Call cannot end while in presenting") {
    runScenario(Call,
      Enqueue("q1")     -> Right(InQueue("q1")),
      TargetAgent("a1") -> Right(Presenting("a1")),
      CallEnded         -> Left(InvalidCallTransition(Presenting("a1"), CallEnded))
    )
  }

  test("An agent cannot reject after accepting a call") {
    runScenario(Call,
        Enqueue("q1")     -> Right(InQueue("q1")),
        TargetAgent("a1") -> Right(Presenting("a1")),
        Accept            -> Right(Processing("a1")),
        Reject            -> Left(InvalidCallTransition(Processing("a1"), Reject))
      )
  }
}

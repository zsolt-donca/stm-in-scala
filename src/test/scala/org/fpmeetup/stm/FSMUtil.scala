package org.fpmeetup.stm

import cats._
import cats.data._
import cats.implicits._

object FSMUtil {

  def runRightScenario[S, T, E](stm: STM[S, T, E], opsAndStates: (T, S)*): Unit = {
    val opsAndRightStates = opsAndStates.map({ case (op, state) => (op, state.asRight[E]) })
    runScenario(stm, opsAndRightStates: _*)
  }

  def runScenario[S, T, E](stm: STM[S, T, E], opsAndErrorOrStates: (T, ErrorOr[S])*) {
    val (ops, errorOrStates) = opsAndErrorOrStates.unzip

    // type inference issues with `Right(stm.init)`
    val actualStates = ops.scanLeft(stm.init.asRight[Any])((either, op) => either.flatMap(st => stm.fun(st, op)))
    assert(actualStates.tail == errorOrStates)
  }

}

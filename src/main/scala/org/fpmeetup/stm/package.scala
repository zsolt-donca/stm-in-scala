package org.fpmeetup

import org.fpmeetup.stm.error.{CallCenterError, CallError}

package object stm {
  type QueueId = String
  type AgentId = String
  type CallId = String

  type ErrorOr[T] = Either[Any, T]

  trait STM[S, T, E] {
    def init: S

    def fun: (S, T) => Either[E, S]
  }
}
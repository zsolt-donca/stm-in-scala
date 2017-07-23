package org.fpmeetup

import org.fpmeetup.examples.MonadTransformers.ErrorAndThenState

package object stm {
  type QueueId = String
  type AgentId = String
  type CallId = String

  type ErrorOr[T] = Either[Any, T]

  trait STM[S, T, E] {
    def init: S

    // override one or the other, or both (if you fancy)
    
    def fun: (S, T) => Either[E, S] = {
      (s, t) => funSt(t).runS(s).value.value
    }

    def funSt: T => ErrorAndThenState[S, E, Unit] = {
      t => ErrorAndThenState(s => fun(s, t).map(s => (s, ())))
    }
  }
}
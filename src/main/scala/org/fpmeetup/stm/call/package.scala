package org.fpmeetup.stm

import org.fpmeetup.examples.MonadTransformers.ErrorAndThenState
import org.fpmeetup.stm.error.{CallError, InvalidCallTransition}

package object call {

  sealed trait CallState

  case object InScript extends CallState
  case class InQueue(queueId: QueueId) extends CallState
  case class Presenting(agentId: AgentId) extends CallState
  case class Processing(agentId: AgentId) extends CallState
  case class PostProcessing(agentId: AgentId) extends CallState
  case object Ended extends CallState

  sealed trait CallOp

  case class Enqueue(queueId: QueueId) extends CallOp
  case object Dequeue extends CallOp
  case class TargetAgent(agentId: AgentId) extends CallOp
  case object Accept extends CallOp
  case object Reject extends CallOp
  case object CallEnded extends CallOp
  case object PostProcessingEnded extends CallOp

  type CallErrorOr[T] = Either[CallError, T]
  type CallSt[T] = ErrorAndThenState[CallState, CallError, T]

  object Call extends STM[CallState, CallOp, CallError] {
    def init: CallState = InScript

    override def fun: (CallState, CallOp) => CallErrorOr[CallState] = {
      case (InScript, Enqueue(queueId)) => Right(InQueue(queueId))
      case (InQueue(_), Dequeue)        => Right(InScript)

      case (InScript | InQueue(_), TargetAgent(agentId)) => Right(Presenting(agentId))

      case (Presenting(agentId), Accept) => Right(Processing(agentId))
      case (Presenting(_), Reject)       => Right(InScript)

      case (Processing(agentId), CallEnded)         => Right(PostProcessing(agentId))
      case (PostProcessing(_), PostProcessingEnded) => Right(Ended)

      case (state, op) => Left(InvalidCallTransition(state, op))
    }
  }
}

package org.fpmeetup

package object stm {

  type QueueId = String
  type AgentId = String

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

  object CallModel {
    def init: CallState = InScript

    def fun: (CallState, CallOp) => Either[Any, CallState] = {
      case (InScript, Enqueue(queueId)) => Right(InQueue(queueId))
      case (InQueue(_), Dequeue)        => Right(InScript)

      case (InScript | InQueue(_), TargetAgent(agentId)) => Right(Presenting(agentId))

      case (Presenting(agentId), Accept) => Right(Processing(agentId))
      case (Presenting(_), Reject)       => Right(InScript)

      case (Processing(agentId), CallEnded)         => Right(PostProcessing(agentId))
      case (PostProcessing(_), PostProcessingEnded) => Right(Ended)

      case (state, op) => Left(InvalidCallModelTransition(state, op))
    }

    /*
    Warning:(30, 49) match may not be exhaustive.
    It would fail on the following inputs: (Ended, Accept), (Ended, CallEnded), (Ended, Dequeue), (Ended, Enqueue(_)), (Ended, PostProcessingEnded), (Ended, Reject), (Ended, TargetAgent(_)), (InScript, Accept), (InScript, CallEnded), (InScript, Dequeue), (InScript, PostProcessingEnded), (InScript, Reject), (PostProcessing(_), Accept), (PostProcessing(_), CallEnded), (PostProcessing(_), Dequeue), (PostProcessing(_), Enqueue(_)), (PostProcessing(_), Reject), (PostProcessing(_), TargetAgent(_)), (Processing(_), Dequeue), (Processing(_), PostProcessingEnded)
    def fun: (CallState, CallOp) => CallState = {
     */
  }

  case class InvalidCallModelTransition(state: CallState, op: CallOp)
}

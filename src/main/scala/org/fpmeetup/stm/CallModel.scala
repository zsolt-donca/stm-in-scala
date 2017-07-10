package org.fpmeetup.stm

import org.fpmeetup.stm.CallModel.{AgentId, QueueId}

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
  type QueueId = String
  type AgentId = String

  def state: (CallState, CallOp) => CallState = {
    case (InScript, Enqueue(queueId)) => InQueue(queueId)
    case (InQueue(_), Dequeue) => InScript

    case (InScript | InQueue(_), TargetAgent(agentId)) => Presenting(agentId)

    case (Presenting(agentId), Accept) => Processing(agentId)
    case (Presenting(_), Reject) => InScript

    case (Processing(agentId), CallEnded) => PostProcessing(agentId)
    case (PostProcessing(_), PostProcessingEnded) => Ended

    case (state, op) => sys.error(s"Invalid operation $op while in state $state")
  }
  
  /*
  Warning:(30, 49) match may not be exhaustive.
  It would fail on the following inputs: (Ended, Accept), (Ended, CallEnded), (Ended, Dequeue), (Ended, Enqueue(_)), (Ended, PostProcessingEnded), (Ended, Reject), (Ended, TargetAgent(_)), (InScript, Accept), (InScript, CallEnded), (InScript, Dequeue), (InScript, PostProcessingEnded), (InScript, Reject), (PostProcessing(_), Accept), (PostProcessing(_), CallEnded), (PostProcessing(_), Dequeue), (PostProcessing(_), Enqueue(_)), (PostProcessing(_), Reject), (PostProcessing(_), TargetAgent(_)), (Processing(_), Dequeue), (Processing(_), PostProcessingEnded)
  def state: (CallState, CallOp) => CallState = {
   */
}
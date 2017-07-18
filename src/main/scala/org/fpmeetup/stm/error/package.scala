package org.fpmeetup.stm

import org.fpmeetup.stm.call.{CallOp, CallState}

package object error {

  sealed trait CallCenterError

  case class DuplicateCall(callId: CallId, callState: CallState) extends CallCenterError
  case class UnknownCall(callId: CallId) extends CallCenterError
  case class CallInProgress(callId: CallId, state: CallState) extends CallCenterError
  case class CallInInvalidStateForTransfer(from: CallState, to: CallState) extends CallCenterError

  
  sealed trait CallError extends CallCenterError

  case class InvalidCallTransition(state: CallState, op: CallOp) extends CallError
}

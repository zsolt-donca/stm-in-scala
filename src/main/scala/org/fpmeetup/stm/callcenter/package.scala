package org.fpmeetup.stm

import cats._
import cats.data._
import cats.implicits._
import org.fpmeetup.stm.call._
import org.fpmeetup.stm.error._

package object callcenter {

  sealed trait CallCenterOp
  case class CallCreated(callId: CallId) extends CallCenterOp
  case class CallUpdated(callId: CallId, callOp: CallOp) extends CallCenterOp
  case class CallDeleted(callId: CallId) extends CallCenterOp
  case class CallTransferred(from: CallId, to: CallId) extends CallCenterOp

  type CallCenterErrorOr[T] = Either[CallCenterError, T]

  type CallCenterState = Map[CallId, CallState]

  object CallCenter extends STM[CallCenterState, CallCenterOp, CallCenterError] {

    override def init: CallCenterState = Map.empty

    override def fun: (CallCenterState, CallCenterOp) => CallCenterErrorOr[CallCenterState] = {
      case (state, CallCreated(callId)) =>
        readCall(state, callId)
          .map(callState => DuplicateCall(callId, callState)).swap
          .map(_ => writeCall(state, callId, Call.init))

      case (state, CallUpdated(callId, callOp)) =>
        for {
          callState <- readCall(state, callId)
          newCallState <- Call.fun(callState, callOp)
        } yield writeCall(state, callId, newCallState)

      case (state, CallDeleted(callId)) =>
        for {
          callState <- readCall(state, callId)
          newState <- callState match {
            case Ended => Right(removeCall(state, callId))
            case _     => Left(CallInProgress(callId, callState))
          }
        } yield newState

      case (state, CallTransferred(fromCallId, toCallId)) =>
        for {
          fromCallState <- readCall(state, fromCallId)
          toCallState   <- readCall(state, toCallId)
          toAgent       <- verifyTransferAndGetAgent(fromCallState, toCallState)
          newState1     = writeCall(state, fromCallId, Processing(toAgent))
          newState2     = writeCall(newState1, toCallId, Ended)
        } yield newState2
    }

    private def verifyTransferAndGetAgent(fromCallState: CallState, toCallState: CallState): CallCenterErrorOr[AgentId] = {
      for {
        toAgent <- (fromCallState, toCallState) match {
          case (Processing(_), Processing(toAgent)) =>
            Right(toAgent)
          case _                                            =>
            Left(CallInInvalidStateForTransfer(fromCallState, toCallState))
        }
      } yield toAgent
    }

    private def readCall(callCenterState: CallCenterState, callId: CallId): CallCenterErrorOr[CallState] = {
      callCenterState.get(callId).toRight(left = UnknownCall(callId))
    }

    private def writeCall(callCenterState: CallCenterState, callId: CallId, callState: CallState): CallCenterState = {
      callCenterState.updated(callId, callState)
    }

    private def removeCall(callCenterState: CallCenterState, callId: CallId): CallCenterState = {
      callCenterState - callId
    }
  }

}

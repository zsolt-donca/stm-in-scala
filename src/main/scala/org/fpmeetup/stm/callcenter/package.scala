package org.fpmeetup.stm

import cats._
import cats.data._
import cats.implicits._
import org.fpmeetup.examples.MonadTransformers.ErrorAndThenState
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

  type CallCenterSt[A] = ErrorAndThenState[CallCenterState, CallCenterError, A]

  object CallCenterSt {
    def apply[A](f: CallCenterState => CallCenterErrorOr[(CallCenterState, A)]): CallCenterSt[A] = {
      ErrorAndThenState(f)
    }
  }

  object CallCenter extends STM[CallCenterState, CallCenterOp, CallCenterError] {

    override def init: CallCenterState = Map.empty

    override def funSt: (CallCenterOp) => CallCenterSt[Unit] = {
      case CallCreated(callId) =>
        createCall(callId)

      case CallUpdated(callId, callOp) =>
        updateCall(callId, callOp)

      case CallDeleted(callId) =>
        deleteCall(callId)

      // in this example, using CallCenterSt only significantly improves Transferred - that's why the above cases stay the same
      case CallTransferred(fromCallId, toCallId) =>
        for {
          fromCall <- readCall(fromCallId)
          toCall   <- readCall(toCallId)
          agent    <- verifyTransferAndGetAgent2(fromCall, toCall)

          _        <- writeCall(fromCallId, Processing(agent))
          _        <- writeCall(toCallId, Ended)
        } yield ()
    }

    private def createCall(callId: CallId): CallCenterSt[Unit] = {
      CallCenterSt(state => {
        readCall(state, callId)
          .map(callState => DuplicateCall(callId, callState))
          .swap
          .map(_ => writeCall(state, callId, Call.init))
          .map(state => (state, ()))
      })
    }

    private def updateCall(callId: CallId, callOp: CallOp): CallCenterSt[Unit] = {
      CallCenterSt(state => {
        for {
          callState <- state.get(callId).toRight(left = UnknownCall(callId))
          newCallState <- Call.funSt(callOp).runS(callState).value.value
          newState = state.updated(callId, newCallState)
        } yield (newState, ())
      })
    }

    private def deleteCall(callId: CallId): CallCenterSt[Unit] = {
      CallCenterSt(state => {
        for {
          callState <- readCall(state, callId)
          newState <- callState match {
            case Ended => Right(removeCall(state, callId))
            case _     => Left(CallInProgress(callId, callState))
          }
        } yield (newState, ())
      })
    }

    private def verifyTransferAndGetAgent2(fromCallState: CallState, toCallState: CallState): CallCenterSt[AgentId] =
      CallCenterSt(state => {
        for {
          toAgent <- (fromCallState, toCallState) match {
            case (Processing(_), Processing(toAgent)) =>
              Right(toAgent)
            case _                                    =>
              Left(CallInInvalidStateForTransfer(fromCallState, toCallState))
          }
        } yield (state, toAgent)
      })

    private def readCall(callCenterState: CallCenterState, callId: CallId): CallCenterErrorOr[CallState] = {
      callCenterState.get(callId).toRight(left = UnknownCall(callId))
    }

    private def readCall(callId: CallId): CallCenterSt[CallState] =
      CallCenterSt(callCenterState => {
        callCenterState.get(callId)
          .toRight(left = UnknownCall(callId))
          .map(callState => (callCenterState, callState))
      })

    private def writeCall(callCenterState: CallCenterState, callId: CallId, callState: CallState): CallCenterState = {
      callCenterState.updated(callId, callState)
    }

    private def writeCall(callId: CallId, callState: CallState): CallCenterSt[Unit] =
      CallCenterSt(callCenterState => {
        Right((callCenterState.updated(callId, callState), ()))
      })

    private def removeCall(callCenterState: CallCenterState, callId: CallId): CallCenterState = {
      callCenterState - callId
    }
  }

}

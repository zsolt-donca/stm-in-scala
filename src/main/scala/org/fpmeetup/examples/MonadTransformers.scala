package org.fpmeetup.examples

import cats.data.{EitherT, StateT}
import cats.{Eval, Now}

object MonadTransformers {

  type ErrorAndThenState[S, E, A] = StateT[EitherT[Eval, E, ?], S, A]
  // S => Either[E, (S, A)]
  // Effect: forgets state on first error;
  //         similar to rolling back a transaction

  type StateAndThenError[S, E, A] = EitherT[StateT[Eval, S, ?], E, A]
  // S => (S, Either[E, A])
  // Effect: carries on state after error;
  //         might update state, even though an error has occurred

  object ErrorAndThenState {
    def apply[S, E, A](f: S => Either[E, (S, A)]): ErrorAndThenState[S, E, A] = {
      StateT[EitherT[Eval, E, ?], S, A](s => EitherT(Now(f(s))))
    }
  }

}

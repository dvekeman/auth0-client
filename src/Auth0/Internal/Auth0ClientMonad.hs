module Auth0.Internal.Auth0ClientMonad where

import           Control.Monad.State

type Auth0ClientM a = StateT [Integer] IO a
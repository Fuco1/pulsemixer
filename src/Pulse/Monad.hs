{-# LANGUAGE MultiParamTypeClasses #-}

module Pulse.Monad
       ( PulseT(..)
       , Pulse
       , runPulse
       , runPulseT
       , getData
       , module Pulse.Monad.Introspect
       , module Pulse.Monad.Context
       ) where

import Control.Concurrent.STM.TVar (newTVarIO, writeTVar)

import Control.Monad (liftM)
import Control.Monad.CatchIO (MonadCatchIO, bracket)
import Control.Monad.RWS.Strict (liftIO, evalRWST, ask)

import Pulse.Monad.Connection (newConn, freeConn)
import Pulse.Monad.Context
import Pulse.Monad.Data (State(..))
import Pulse.Monad.Monad (PulseT(..), Pulse, getData)
import Pulse.Monad.Introspect

runPulseT :: MonadCatchIO m => PulseT m n -> m n
runPulseT (PulseT code) = bracket
                          (do env <- liftIO newConn
                              tvar <- liftIO $ newTVarIO (State 0)
                              return (env, tvar))
                          (liftIO . freeConn . ask . fst)
                          $ \(env, tvar) -> fst `liftM` evalRWST code env tvar

runPulse :: Pulse n -> IO n
runPulse = runPulseT

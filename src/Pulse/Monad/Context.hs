{-# Language TypeSynonymInstances, FlexibleInstances #-}

module Pulse.Monad.Context
       ( setSinkInputVolume
       ) where

import Foreign.C

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Control.Monad (liftM, void)
import Control.Monad.IO.Class (liftIO)

-- import Foreign.C
import Foreign.Safe
-- import Foreign.C.String

import Foreign.StablePtr (newStablePtr, deRefStablePtr)
import Foreign.Ptr (FunPtr(..))
import Foreign.Storable

import Pulse.Monad.Data
import Pulse.Monad.Monad

import Pulse.Internal.C2HS (castPtrToMaybeStable, RawUserData)
import Pulse.Internal.Context
import Pulse.Internal.Introspect

synchronizingCallback :: MVar Bool -> IO (FunPtr (RawContextSuccessCallback a))
synchronizingCallback mvar = wrapRawContextSuccessCallback $ \_ success _ -> putMVar mvar (success > 0)

setSinkInputVolume :: Integer -> SinkInput -> Pulse Bool
setSinkInputVolume vol (SinkInput { sinkInputIndex = index
                                  , sinkInputVolume = (CVolume _ p)
                                  }) = do
  ctx <- getContext
  let v = CVolume [vol, vol] p
  liftIO $ alloca $ \ptr -> do
    mvar <- newEmptyMVar
    fptr <- synchronizingCallback mvar
    poke ptr v
    contextSetSinkInputVolume ctx index ptr fptr Nothing
    takeMVar mvar

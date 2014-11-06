{-# Language TypeSynonymInstances, FlexibleInstances #-}

module Pulse.Monad.Context
       ( MuteCommand(..)
       , setSinkInputVolume
       , setSinkInputMute
       ) where

import Foreign.C

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Control.Monad (liftM, void)
import Control.Monad.IO.Class (liftIO)


import Foreign.Safe hiding (void)
import Foreign.StablePtr (newStablePtr, deRefStablePtr)
import Foreign.Ptr (FunPtr(..))
import Foreign.Storable

import Pulse.Monad.Data
import Pulse.Monad.Monad

import Pulse.Internal.C2HS (castPtrToMaybeStable, RawUserData)
import Pulse.Internal.Context
import Pulse.Internal.Introspect

import System.Timeout

synchronizingCallback :: MVar Bool -> IO (FunPtr (RawContextSuccessCallback a))
synchronizingCallback mvar = wrapRawContextSuccessCallback $ \_ success _ -> putMVar mvar (success > 0)

-- ^ This should call a "action" syncrhonously, and return its success status
callSynchronously :: (FunPtr (RawContextSuccessCallback a) -> IO ()) -> IO Bool
callSynchronously action = do
  mvar <- newEmptyMVar
  fptr <- synchronizingCallback mvar
  action fptr
  re <- timeout 1000000 $ takeMVar mvar
  return $ case re of
    Just x -> x
    Nothing -> False

setSinkInputVolume :: Integer -> SinkInput -> Pulse Bool
setSinkInputVolume vol (SinkInput { sinkInputIndex = index
                                  , sinkInputVolume = (CVolume _ p)
                                  }) = do
  ctx <- getContext
  let v = CVolume [vol, vol] p
  liftIO $ alloca $ \ptr -> do
    poke ptr v
    callSynchronously $ \fptr -> void $ contextSetSinkInputVolume ctx index ptr fptr Nothing

data MuteCommand = Mute | Unmute | Toggle deriving (Eq, Ord, Show)

setSinkInputMute :: MuteCommand -> SinkInput -> Pulse Bool
setSinkInputMute cmd (SinkInput { sinkInputIndex = index
                                , sinkInputMute = mute
                                }) = do
  ctx <- getContext
  liftIO $ do
    callSynchronously $ \fptr ->
      void $ case cmd of
              Mute -> contextSetSinkInputMute ctx index 1 fptr Nothing
              Unmute -> contextSetSinkInputMute ctx index 0 fptr Nothing
              Toggle -> contextSetSinkInputMute ctx index (if mute == Muted then 0 else 1) fptr Nothing

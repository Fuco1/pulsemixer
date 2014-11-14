{-# Language TypeSynonymInstances, FlexibleInstances #-}

module Pulse.Monad.Introspect
       ( SinkInputInfoCallback
       , getSinkInputInfoList
       ) where

import Foreign.C

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Control.Exception

import Control.Monad (liftM, void)
import Control.Monad.IO.Class (liftIO)

import Foreign.StablePtr (newStablePtr, deRefStablePtr)
import Foreign.Ptr (FunPtr(..))
import Foreign.Storable

import Pulse.Monad.Data
import Pulse.Monad.Monad

import Pulse.Internal.C2HS (castPtrToMaybeStable, RawUserData)
import Pulse.Internal.Context
import Pulse.Internal.Introspect

import System.Timeout

class CallbackInfo i where
  wrapRaw :: RawInfoCallback i u -> IO (RawInfoCallbackPtr i u)

  wrap :: InfoCallback i u -> IO (RawInfoCallbackPtr i u)
  wrap = wrapInfoCallback

instance CallbackInfo RawSinkInputInfoPtr where
  wrapRaw = wrapRawSinkInputInfoCallback

instance CallbackInfo RawSinkInfoPtr where
  wrapRaw = wrapRawSinkInfoCallback

wrapInfoCallback :: CallbackInfo i => InfoCallback i u -> IO (RawInfoCallbackPtr i u)
wrapInfoCallback cb = wrapRaw $ \ctx info eol userdata -> do
  d <- case castPtrToMaybeStable userdata of
        Just ptr -> Just `liftM` deRefStablePtr ptr
        Nothing -> return Nothing
  cb ctx info (eol > 0) d

synchronizeCallback :: CallbackInfo i => MVar () -> TVar a -> (TVar a -> i -> IO ()) -> IO (RawInfoCallbackPtr i u)
synchronizeCallback mvar tvar action = flip onException (putMVar mvar ()) $ wrap $ \ctx info eol userdata -> do
  empty <- isEmptyMVar mvar
  if empty && not eol
    then action tvar info
    else putMVar mvar ()

callSynchronously :: CallbackInfo i => a -> (TVar a -> i -> IO ()) -> (RawInfoCallbackPtr i u -> IO ()) -> IO a
callSynchronously zero cb action = do
  mvar <- newEmptyMVar
  tvar <- newTVarIO zero
  synchronizeCallback mvar tvar cb >>= action
  timeout 1000000 $ takeMVar mvar
  atomically $ readTVar tvar

processSinkInput :: TVar [SinkInput] -> RawSinkInputInfoPtr -> IO ()
processSinkInput tvar info = do
  RawSinkInputInfo { index'RawSinkInputInfo = index
                   , name'RawSinkInputInfo = Just name
                   , volume'RawSinkInputInfo = volume
                   , mute'RawSinkInputInfo = mute
                   , proplist'RawSinkInputInfo = rawPL
                   } <- peek info
  pl <- propListFromRaw rawPL
  atomically $ modifyTVar tvar ((SinkInput
                                 index
                                 name
                                 volume
                                 (if (mute > 0) then Muted else Unmuted)
                                 pl) :)

getSinkInputInfoList :: Pulse [SinkInput]
getSinkInputInfoList = do
  ctx <- getContext
  liftIO $ callSynchronously [] processSinkInput (\cb -> void $ contextGetSinkInputInfoList ctx cb Nothing)

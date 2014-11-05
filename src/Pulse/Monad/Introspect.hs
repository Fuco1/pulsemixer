module Pulse.Monad.Introspect
       ( SinkInputInfoCallback
       , getSinkInputInfoList
       , getSinkInputInfoListSync
       ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Control.Monad (liftM, void)
import Control.Monad.IO.Class (liftIO)

import Foreign.StablePtr (newStablePtr, deRefStablePtr)
import Foreign.Ptr (FunPtr(..))
import Foreign.Storable

import Pulse.Monad.Data
import Pulse.Monad.Monad

import Pulse.Internal.C2HS (castPtrToMaybeStable)
import Pulse.Internal.Context
import Pulse.Internal.Introspect

type SinkInputInfoCallback a = RawContextPtr -> RawSinkInputInfoPtr -> Bool -> Maybe a -> IO ()

wrapSinkInputInfoCallback :: SinkInputInfoCallback a -> IO (FunPtr (RawSinkInputInfoCallback a))
wrapSinkInputInfoCallback cb = wrapRawSinkInputInfoCallback $ \ctx info eol userdata -> do
  d <- case castPtrToMaybeStable userdata of
        Just ptr -> Just `liftM` deRefStablePtr ptr
        Nothing -> return Nothing
  cb ctx info (eol > 0) d

getSinkInputInfoList :: SinkInputInfoCallback a -> Pulse ()
getSinkInputInfoList cb = do
  ctx <- getContext
  dat <- getData
  liftIO $ do
    udata <- Just `liftM` newStablePtr dat
    cbWrapped <- wrapSinkInputInfoCallback cb
    contextGetSinkInputInfoList ctx cbWrapped udata
    return ()

sinkInputCbSync :: MVar () -> TVar [SinkInput] -> SinkInputInfoCallback ()
sinkInputCbSync mvar tvar ctx info eol userdata = do
  if eol
    then putMVar mvar ()
    else do
      RawSinkInputInfo { index'RawSinkInputInfo = index
                       , sinkInputName'RawSinkInputInfo = Just name
                       , proplist'RawSinkInputInfo = rawPL
                       } <- peek info
      pl <- propListFromRaw rawPL
      let new = SinkInput index name pl
      atomically $ modifyTVar tvar (new :)

getSinkInputInfoListSync :: Pulse [SinkInput]
getSinkInputInfoListSync = do
  ctx <- getContext
  liftIO $ do
    mvar <- (newEmptyMVar :: IO (MVar ()))
    tvar <- newTVarIO []
    cbWrapped <- wrapSinkInputInfoCallback (sinkInputCbSync mvar tvar)
    contextGetSinkInputInfoList ctx cbWrapped Nothing
    status <- readMVar mvar
    sinks <- atomically $ readTVar tvar
    return sinks

module Pulse.Monad.Introspect
       ( SinkInputInfoCallback
       , getSinkInputInfoList
       ) where

import Control.Monad (liftM)
import Control.Monad.IO.Class (liftIO)

import Foreign.StablePtr (newStablePtr, deRefStablePtr)
import Foreign.Ptr (FunPtr(..))

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

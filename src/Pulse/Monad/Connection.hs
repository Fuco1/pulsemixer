module Pulse.Monad.Connection
       ( newConn
       , freeConn
       ) where

import Foreign.Ptr (FunPtr(..), nullPtr)

import Control.Concurrent.MVar

import Pulse.Monad.Data

import Pulse.Internal.Context
import Pulse.Internal.MainLoopApi
import Pulse.Internal.ThreadedMainLoop

notifyOnReady :: MVar () -> IO (FunPtr (RawContextNotifyCallback ()))
notifyOnReady mvar =
  wrapRawContextNotifyCallback $ \ctx _ -> do
    state <- contextGetState ctx
    case state of
     ContextReady -> putMVar mvar ()
     otherwise -> return ()

newConn :: IO Env
newConn = do
  ml <- threadedMainloopNew
  api <- threadedMainloopGetApi ml
  ctx <- contextNew api (Just "haskell-pa-test")
  mvar <- newEmptyMVar
  cb <- notifyOnReady mvar
  contextSetStateCallback ctx cb Nothing
  contextConnect ctx Nothing 0 nullPtr
  mlStatus <- threadedMainloopStart ml
  takeMVar mvar
  return (Env ctx ml)

freeConn :: Env -> IO ()
freeConn Env { envContext = ctx
             , envMainThread = ml
             } = do
  contextDisconnect ctx
  contextUnref ctx
  threadedMainloopStop ml
  threadedMainloopFree ml

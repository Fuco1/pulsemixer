module Pulse.Monad.Connection
       ( newConn
       , freeConn
       ) where

import Foreign.Ptr (nullPtr)

import Pulse.Monad.Data

import Pulse.Internal.Context
import Pulse.Internal.MainLoopApi
import Pulse.Internal.ThreadedMainLoop

newConn :: IO Env
newConn = do
  ml <- threadedMainloopNew
  api <- threadedMainloopGetApi ml
  ctx <- contextNew api (Just "haskell-pa-test")
  contextConnect ctx Nothing 0 nullPtr
  mlStatus <- threadedMainloopStart ml
  return (Env ctx ml)

freeConn :: Env -> IO ()
freeConn Env { envContext = ctx
             , envMainThread = ml
             } = do
  contextDisconnect ctx
  contextUnref ctx
  threadedMainloopStop ml
  threadedMainloopFree ml

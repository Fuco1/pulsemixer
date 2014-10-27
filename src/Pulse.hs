{-# LANGUAGE ForeignFunctionInterface #-}

module Pulse where

import GHC.IO.Encoding (utf8)
import qualified GHC.Foreign as GHC

import Foreign.C
import Foreign.Safe
import Foreign.C.String
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable

import UI.NCurses

import Control.Applicative
import Control.Concurrent.MVar
import Control.Concurrent.STM.TVar (TVar(..), newTVarIO, writeTVar, readTVar, modifyTVar)
import Control.Concurrent.STM (atomically)
import Control.Monad
import Control.Monad.Fix
import Control.Monad.RWS.Strict

import qualified Data.ByteString as B
import Data.ByteString.UTF8 (toString)
import Data.Maybe

import Pulse.Internal.C2HS
import Pulse.Internal.Context
import Pulse.Internal.Def
import Pulse.Internal.MainLoopApi
import Pulse.Internal.Operation
import Pulse.Internal.PropList
import Pulse.Internal.ThreadedMainLoop
import Pulse.Internal.Introspect

import Pulse.Monad
import Pulse.Monad.Data

sinkInputCb :: SinkInputInfoCallback (TVar State)
sinkInputCb ctx info eol userdata = do
  when (isJust userdata) $ do
    let Just dat = userdata
    num <- atomically $ nSinks `liftM` readTVar dat
    putStrLn $ "Number of calls to info: " ++ show num
    atomically $ modifyTVar dat (\(State x) -> (State (x+1)))
  if eol
    then putStrLn "Done"
    else do
      RawSinkInputInfo { sinkInputName'RawSinkInputInfo = Just name
                       , volume'RawSinkInputInfo = (CVolume volumes _)
                       , proplist'RawSinkInputInfo = proplist } <- peek info
      putStrLn $ "Name of device: " ++ name
      Just appName <- proplistGets proplist "application.name"
      putStrLn $ "Application name: " ++ appName ++ "\nVolume: " ++ show volumes

notifyCB :: RawContextNotifyCallback a
notifyCB ctx _ = show <$> contextGetState ctx >>= putStrLn

run :: Pulse ()
run = do
  cmd <- liftIO $ getLine
  case cmd of
    "quit" -> liftIO $ putStrLn "Quitting"
    "info" -> getSinkInputInfoList sinkInputCb >> run
    "m" -> do
      dat <- getData
      num <- liftIO $ atomically $ nSinks `liftM` readTVar dat
      liftIO $ putStrLn $ "Number of calls to info: " ++ show num
      run
    _ -> run

main :: IO ()
main = runPulse run

-- Local Variables:
-- haskell-program-name: "ghci -lpulse"
-- End:

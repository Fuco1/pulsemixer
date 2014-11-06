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

notifyCB :: RawContextNotifyCallback a
notifyCB ctx _ = show <$> contextGetState ctx >>= putStrLn

run :: Pulse ()
run = do
  cmd <- liftIO $ getLine
  case cmd of
    "quit" -> liftIO $ putStrLn "Quitting"
    "info" -> getSinkInputInfoList >>= liftIO . putStrLn . show >> run
    "m" -> do
      dat <- getData
      num <- liftIO $ atomically $ nSinks `liftM` readTVar dat
      liftIO $ putStrLn $ "Number of calls to info: " ++ show num
      run
    "set" -> do
      value <- liftIO $ read `liftM` getLine
      (si:_) <- getSinkInputInfoList
      setSinkInputVolume value si
      run
    _ -> run

main :: IO ()
main = runPulse run

-- Local Variables:
-- haskell-program-name: "ghci -lpulse"
-- End:

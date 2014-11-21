module Pulse.Monad.Data
       ( Env(..)
       , State(..)
       , MuteState(..)
       , SinkInput(..)
       , Sink(..)
       , PropList(..)
       , RawCVolume(..)
       , Volume
       , propListFromRaw
       ) where

import Data.Map as M

import Pulse.Internal.Context (RawContextPtr)
import Pulse.Internal.PropList (RawPropListPtr, proplistGets)
import Pulse.Internal.ThreadedMainLoop (RawThreadedMainLoopPtr)
import Pulse.Internal.Volume (RawCVolume(..), Volume)

data Env = Env
           { envContext :: RawContextPtr
           , envMainThread :: RawThreadedMainLoopPtr
           }

data Properties = ApplicationName deriving (Eq, Ord, Show)

type PropList = Map Properties (Maybe String)

propListFromRaw :: RawPropListPtr -> IO PropList
propListFromRaw proplist = do
  appName <- proplistGets proplist "application.name"
  return $ M.insert ApplicationName appName .
    id $ M.empty

data MuteState = Muted | Unmuted deriving (Eq, Ord, Show)

data SinkInput = SinkInput
                 { sinkInputIndex :: Int
                 , sinkInputName :: String
                 , sinkInputVolume :: RawCVolume
                 , sinkInputMute :: MuteState
                 , sinkInputProplist :: PropList
                 } deriving Show

data Sink = Sink
            { sinkIndex :: Int
            , sinkName :: String
            , sinkVolume :: RawCVolume
            , sinkMute :: MuteState
            , sinkProplist :: PropList
            } deriving Show

data State = State
             { nSinks :: Int
             }

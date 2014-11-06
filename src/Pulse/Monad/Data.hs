module Pulse.Monad.Data
       ( Env(..)
       , State(..)
       , SinkInput(..)
       , PropList(..)
       , propListFromRaw
       ) where

import Data.Map as M

import Pulse.Internal.Context (RawContextPtr)
import Pulse.Internal.PropList (RawPropListPtr, proplistGets)
import Pulse.Internal.ThreadedMainLoop (RawThreadedMainLoopPtr)
import Pulse.Internal.Volume (RawCVolume)

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

data SinkInput = SinkInput
                 { sinkInputIndex :: Int
                 , sinkInputName :: String
                 , sinkInputVolume :: RawCVolume
                 , sinkInputProplist :: PropList
                 } deriving Show

data State = State
             { nSinks :: Int
             }

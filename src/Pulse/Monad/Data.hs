module Pulse.Monad.Data
       ( Env(..)
       , State(..)
       ) where

import Pulse.Internal.Context (RawContextPtr)
import Pulse.Internal.ThreadedMainLoop (RawThreadedMainLoopPtr)

data Env = Env
           { envContext :: RawContextPtr
           , envMainThread :: RawThreadedMainLoopPtr
           }

data State = State
             { nSinks :: Int
             }

{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

module Pulse.Internal.ThreadedMainLoop where

import Foreign.C
import Foreign.Safe
import Pulse.Internal.C2HS

{#import Pulse.Internal.MainLoopApi #}

#include <pulse/thread-mainloop.h>

data RawThreadedMainLoop
{#pointer *threaded_mainloop as RawThreadedMainLoopPtr -> RawThreadedMainLoop #}

{#fun threaded_mainloop_new as ^ {} -> `RawThreadedMainLoopPtr' id #}

{#fun threaded_mainloop_get_api as ^ {id `RawThreadedMainLoopPtr'} -> `RawMainLoopApiPtr' id #}

{#fun threaded_mainloop_start as ^ {id `RawThreadedMainLoopPtr'} -> `Int' #}

-- Local Variables:
-- mode: haskell
-- End:

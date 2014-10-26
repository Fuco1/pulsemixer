{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

module Pulse.Internal.MainLoopApi where

import Foreign.Safe
import Foreign.C

#include <pulse/mainloop-api.h>

data RawMainLoopApi
{#pointer *pa_mainloop_api as RawMainLoopApiPtr -> RawMainLoopApi #}

-- Local Variables:
-- mode: haskell
-- End:

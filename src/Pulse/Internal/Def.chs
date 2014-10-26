{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

module Pulse.Internal.Def where

import Foreign.Safe
import Foreign.C

#include <pulse/def.h>

data SpawnApi
{#pointer *pa_spawn_api as SpawnApiPtr -> SpawnApi #}

-- Local Variables:
-- mode: haskell
-- End:

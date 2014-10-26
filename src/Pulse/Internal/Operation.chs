{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

module Pulse.Internal.Operation where

import Foreign.Safe
import Foreign.C

#include <pulse/operation.h>

data RawOperation
{#pointer *pa_operation as RawOperationPtr -> RawOperation #}

{#fun operation_unref as ^ {id `RawOperationPtr' } -> `()' id #}

-- Local Variables:
-- mode: haskell
-- End:

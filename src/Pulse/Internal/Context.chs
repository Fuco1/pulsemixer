{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

module Pulse.Internal.Context where

import Foreign.Safe
import Foreign.C
import Pulse.Internal.C2HS

{#import Pulse.Internal.Operation #}
{#import Pulse.Internal.MainLoopApi #}
{#import Pulse.Internal.PropList #}
{#import Pulse.Internal.Def #}

#include <pulse/context.h>

type RawContextNotifyCallback a = RawContextPtr -> RawUserData a -> IO ()
type RawContextSuccessCallback a = RawContextPtr -> CInt -> RawUserData a -> IO ()

foreign import ccall "wrapper" wrapRawContextNotifyCallback :: RawContextNotifyCallback a  -> IO (FunPtr (RawContextNotifyCallback a))
foreign import ccall "wrapper" wrapRawContextSuccessCallback :: RawContextSuccessCallback a  -> IO (FunPtr (RawContextSuccessCallback a))

data RawContext
{#pointer *pa_context as RawContextPtr -> RawContext #}

{#fun context_new as ^
    { id `RawMainLoopApiPtr'
    , withNullableUTF8CString* `Maybe String'
    } -> `RawContextPtr' id #}

{#fun context_connect as ^
    { id `RawContextPtr'
    , withNullableUTF8CString* `Maybe String'
    , `Int'
    , id `SpawnApiPtr'
    } -> `Int' #}

{#fun context_disconnect as ^ {id `RawContextPtr'} -> `()' id #}

{#fun context_unref as ^ {id `RawContextPtr'} -> `()' id #}

{#fun context_set_state_callback as ^
    { id `RawContextPtr'
    , id `FunPtr (RawContextNotifyCallback a)'
    , castMaybeStablePtrToPtr `UserData a'
    } -> `()' #}

{#enum context_state as ^ {underscoreToCase} deriving (Eq, Show)#}

{#fun context_get_state as ^ { id `RawContextPtr'} -> `ContextState' cToEnum #}

-- Local Variables:
-- mode: haskell
-- End:

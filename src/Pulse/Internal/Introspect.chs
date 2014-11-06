{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

module Pulse.Internal.Introspect
       ( module Pulse.Internal.Introspect
       , RawCVolume(..)
       ) where

import Foreign.C
import Foreign.Ptr
import Foreign.Safe
import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM)
import Pulse.Internal.C2HS
{#import Pulse.Internal.PropList #}
{#import Pulse.Internal.Context #}
{#import Pulse.Internal.Operation #}
{#import Pulse.Internal.Volume #}

#include <pulse/introspect.h>

-- TODO: make better names for Callbacks, actions, TVar a -> i -> IO ()
-- things etc... right now the functions are quite confusing
type InfoCallback i u = RawContextPtr -> i -> Bool -> Maybe u -> IO ()
type RawInfoCallback i u = RawContextPtr -> i -> CInt -> RawUserData u -> IO ()
type RawInfoCallbackPtr i u = FunPtr (RawContextPtr -> i -> CInt -> RawUserData u -> IO ())

type SinkInputInfoCallback a = InfoCallback RawSinkInputInfoPtr a
type RawSinkInputInfoCallback a = RawInfoCallback RawSinkInputInfoPtr a

foreign import ccall "wrapper" wrapRawSinkInputInfoCallback :: RawSinkInputInfoCallback a -> IO (FunPtr (RawSinkInputInfoCallback a))

data RawSinkInputInfo = RawSinkInputInfo
    { index'RawSinkInputInfo :: Int
    , sinkInputName'RawSinkInputInfo :: Maybe String
    , volume'RawSinkInputInfo :: RawCVolume
    , driverName'RawSinkInputInfo :: Maybe String
    , proplist'RawSinkInputInfo :: RawPropListPtr
    }

instance Storable RawSinkInputInfo where
    sizeOf _ = {#sizeof pa_sink_input_info #}
    alignment _ = {#alignof pa_sink_input_info #}
    peek p = RawSinkInputInfo
        <$> cIntConv `liftM` ({#get pa_sink_input_info->index #} p)
        <*> (peekNullableUTF8CString =<< ({#get pa_sink_input_info->name #} p))
        <*> peek ((p `plusPtr` 172) :: Ptr RawCVolume)
        <*> (peekNullableUTF8CString =<< ({#get pa_sink_input_info->driver #} p))
        <*> ({#get pa_sink_input_info->proplist #} p)
    poke p x = return ()
{#pointer *sink_input_info as RawSinkInputInfoPtr -> RawSinkInputInfo #}

{#fun context_get_sink_input_info as ^
    { id `RawContextPtr',
      `Int',
      id `FunPtr (RawSinkInputInfoCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr' id #}

{#fun context_get_sink_input_info_list as ^
    { id `RawContextPtr',
      id `FunPtr (RawSinkInputInfoCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr' id #}

{#fun context_set_sink_input_volume as ^
    { id `RawContextPtr',
      `Int',
      id `RawCVolumePtr',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr' id #}

{#fun context_set_sink_input_mute as ^
    { id `RawContextPtr',
      `Int',
      `Int',
      id `FunPtr (RawContextSuccessCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr' id #}

-- Local Variables:
-- mode: haskell
-- End:

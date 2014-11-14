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

type SinkInfoCallback a = InfoCallback RawSinkInfoPtr a
type RawSinkInfoCallback a = RawInfoCallback RawSinkInfoPtr a

foreign import ccall "wrapper" wrapRawSinkInputInfoCallback :: RawSinkInputInfoCallback a -> IO (FunPtr (RawSinkInputInfoCallback a))
foreign import ccall "wrapper" wrapRawSinkInfoCallback :: RawSinkInfoCallback a -> IO (FunPtr (RawSinkInfoCallback a))

data RawSinkInputInfo = RawSinkInputInfo
    { index'RawSinkInputInfo :: Int
    , name'RawSinkInputInfo :: Maybe String
    , volume'RawSinkInputInfo :: RawCVolume
    , driverName'RawSinkInputInfo :: Maybe String
    , mute'RawSinkInputInfo :: Int
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
        <*> cIntConv `liftM` ({#get pa_sink_input_info->mute #} p)
        <*> ({#get pa_sink_input_info->proplist #} p)
    poke p x = return ()
{#pointer *sink_input_info as RawSinkInputInfoPtr -> RawSinkInputInfo #}

data RawSinkInfo = RawSinkInfo
    { name'RawSinkInfo :: Maybe String
    , index'RawSinkInfo :: Int
    , volume'RawSinkInfo :: RawCVolume
    , mute'RawSinkInfo :: Int
    , proplist'RawSinkInfo :: RawPropListPtr
    }

instance Storable RawSinkInfo where
    sizeOf _ = {#sizeof pa_sink_info #}
    alignment _ = {#alignof pa_sink_info #}
    peek p = RawSinkInfo
        <$> (peekNullableUTF8CString =<< ({#get pa_sink_info->name #} p))
        <*> cIntConv `liftM` ({#get pa_sink_info->index #} p)
        <*> peek ((p `plusPtr` 172) :: Ptr RawCVolume)
        <*> cIntConv `liftM` ({#get pa_sink_info->mute #} p)
        <*> ({#get pa_sink_info->proplist #} p)
    poke p x = return ()
{#pointer *sink_info as RawSinkInfoPtr -> RawSinkInfo #}

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

{#fun context_get_sink_info_by_index as ^
    { id `RawContextPtr',
      `Int',
      id `FunPtr (RawSinkInfoCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr' id #}

{#fun context_get_sink_info_list as ^
    { id `RawContextPtr',
      id `FunPtr (RawSinkInfoCallback a)',
      castMaybeStablePtrToPtr `UserData a'
    } -> `RawOperationPtr' id #}

-- Local Variables:
-- mode: haskell
-- End:

{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

module Pulse.Internal.PropList
    (
    RawPropList,
    RawPropListPtr,
    proplistNew,
    proplistFree,
    proplistSet,
    proplistGet,
    proplistGets,
    PropListIterateState,
    proplistIterate,
    proplistFromString
    ) where

import Control.Exception (throwIO, ErrorCall(..))
import Data.ByteString (ByteString)
import Foreign.Safe
import Foreign.C

import Pulse.Internal.C2HS

#include <pulse/proplist.h>

data RawPropList
{#pointer *pa_proplist as RawPropListPtr -> RawPropList #}

{#fun proplist_new as ^ {} -> `RawPropListPtr' id #}

{#fun proplist_free as ^ {id `RawPropListPtr' } -> `()' id #}

proplistSet :: RawPropListPtr -> String -> ByteString -> IO ()
proplistSet raw key val = do
    ret <- proplistSet' raw key val
    if ret /= 0
        then throwIO $ ErrorCall "invalid key"
        else return ()

{#fun proplist_set as proplistSet' {id `RawPropListPtr', withUTF8CString* `String', useAsCStringLen'* `ByteString'&} -> `Int' #}

proplistGet :: RawPropListPtr -> String -> IO ByteString
proplistGet raw key = do
    (ret, cstr, len) <- proplistGet' raw key
    if ret /= 0
        then throwIO $ ErrorCall "not found"
        else packCStringLen (cstr, len)

{#fun proplist_get as proplistGet'
    { id `RawPropListPtr'
    , withUTF8CString* `String'
    , alloca- `CString' castPtr
    , alloca- `Int' peekIntConv*
    } -> `Int' #}

{#fun proplist_gets as proplistGets
    { id `RawPropListPtr'
    , withUTF8CString* `String'
    } -> `Maybe String' peekNullableUTF8CString* #}

type PropListIterateState = Ptr ()

{#fun proplist_iterate as ^ {id `RawPropListPtr', id `Ptr PropListIterateState'} -> `Maybe String' peekNullableUTF8CString* #}

{#fun proplist_from_string as ^ {withUTF8CString * `String'} -> `Maybe RawPropListPtr' toMaybePtr #}

-- Local Variables:
-- mode: haskell
-- End:

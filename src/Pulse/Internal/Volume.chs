{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

{#context prefix = "pa"#}

module Pulse.Internal.Volume where

import Foreign.Safe
import Foreign.C
import Foreign.Marshal.Array

import Control.Monad (liftM)

import Pulse.Internal.C2HS

#include <pulse/volume.h>

type Volume = Integer

data RawCVolume = CVolume
    { channeVolumes :: [Volume]
    , volRaw :: RawCVolumePtr
    }

instance Storable RawCVolume where
    sizeOf _ = {#sizeof pa_cvolume #}
    alignment _ = {#alignof pa_cvolume #}
    peek p = do
        channelNum <- liftM cIntConv ({#get pa_cvolume->channels #} p)
        rawArrayHead <- ({#get pa_cvolume->values #} p)
        channelVolumes <- return . map cIntConv =<< peekArray channelNum rawArrayHead
        return $ CVolume channelVolumes p
    poke p (CVolume vol raw) = do
        {#set pa_cvolume.channels #} p undefined
        {#set pa_cvolume.values #} p undefined

{#pointer *pa_cvolume as RawCVolumePtr -> RawCVolume #}

-- Local Variables:
-- mode: haskell
-- End:

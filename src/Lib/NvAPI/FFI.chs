{-# LANGUAGE GeneralizedNewtypeDeriving   #-}
{-# LANGUAGE MagicHash   #-}
{-# LANGUAGE EmptyDataDecls   #-}
{-# LANGUAGE RecordWildCards   #-}
module Lib.NvAPI.FFI where

import Control.Exception (ioError)
import Data.Bits
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (with)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import System.IO.Unsafe (unsafePerformIO)
import System.IO.Error
import Data.Coerce (coerce)

#include "nvapi.h"


_NVAPI_MAX_PHYSICAL_GPUS :: Int
_NVAPI_MAX_PHYSICAL_GPUS = {#const NVAPI_MAX_PHYSICAL_GPUS#}

_NVAPI_SHORT_STRING_MAX :: Int
_NVAPI_SHORT_STRING_MAX = {#const NVAPI_SHORT_STRING_MAX#}

foreign import ccall unsafe "c_NV_SCANOUT_INTENSITY_DATA_VER" _NV_SCANOUT_INTENSITY_DATA_VER :: NvU32
foreign import ccall unsafe "c_NV_SCANOUT_WARPING_VER" _NV_SCANOUT_WARPING_VER :: NvU32
foreign import ccall unsafe "c_NV_SCANOUT_WARPING_STATE_VER" _NV_SCANOUT_WARPING_STATE_VER :: NvU32
foreign import ccall unsafe "c_NV_SCANOUT_INTENSITY_STATE_VER" _NV_SCANOUT_INTENSITY_STATE_VER :: NvU32
foreign import ccall unsafe "c_NV_SCANOUT_INFORMATION_VER" _NV_SCANOUT_INFORMATION_VER :: NvU32
foreign import ccall unsafe "c_NV_GPU_DISPLAYIDS_VER" _NV_GPU_DISPLAYIDS_VER :: NvU32


{#pointer NvPhysicalGpuHandle as NvPhysicalGpuHandle #}
{#pointer NvDisplayHandle as NvDisplayHandle #}
{#pointer NvVioHandle as NvVioHandle #}

{#enum NvAPI_Status as NvAPIStatus {} deriving (Show, Eq) #}
{#enum NV_GPU_SCANOUT_COMPOSITION_PARAMETER {} deriving (Show, Read, Eq) #}
{#enum NV_GPU_SCANOUT_COMPOSITION_PARAMETER_VALUE {} deriving (Show, Read, Eq) #}
{#enum NV_ROTATE {} deriving (Show, Read, Eq) #}
{#enum NV_GPU_WARPING_VERTICE_FORMAT {} deriving (Show, Read, Eq) #}
{#enum NV_MONITOR_CONN_TYPE {} deriving (Show, Read, Eq) #}

instance Storable NV_GPU_SCANOUT_COMPOSITION_PARAMETER where
  sizeOf _ = {#sizeof NV_GPU_SCANOUT_COMPOSITION_PARAMETER #}
  alignment _ = {#alignof NV_GPU_SCANOUT_COMPOSITION_PARAMETER #}
  peek p = toEnum' <$> peek (castPtr p :: Ptr {#type NV_GPU_SCANOUT_COMPOSITION_PARAMETER #})
  poke p = poke (castPtr p :: Ptr {#type NV_GPU_SCANOUT_COMPOSITION_PARAMETER #}) . fromEnum'

instance Storable NV_GPU_SCANOUT_COMPOSITION_PARAMETER_VALUE where
  sizeOf _ = {#sizeof NV_GPU_SCANOUT_COMPOSITION_PARAMETER_VALUE #}
  alignment _ = {#alignof NV_GPU_SCANOUT_COMPOSITION_PARAMETER_VALUE #}
  peek p = toEnum' <$> peek (castPtr p :: Ptr {#type NV_GPU_SCANOUT_COMPOSITION_PARAMETER_VALUE #})
  poke p = poke (castPtr p :: Ptr {#type NV_GPU_SCANOUT_COMPOSITION_PARAMETER_VALUE #}) . fromEnum'

instance Storable NV_ROTATE where
  sizeOf _ = {#sizeof NV_ROTATE #}
  alignment _ = {#alignof NV_ROTATE #}
  peek p = toEnum' <$> peek (castPtr p :: Ptr {#type NV_ROTATE #})
  poke p = poke (castPtr p :: Ptr {#type NV_ROTATE #}) . fromEnum'

instance Storable NV_GPU_WARPING_VERTICE_FORMAT where
  sizeOf _ = {#sizeof NV_GPU_WARPING_VERTICE_FORMAT #}
  alignment _ = {#alignof NV_GPU_WARPING_VERTICE_FORMAT #}
  peek p = toEnum' <$> peek (castPtr p :: Ptr {#type NV_GPU_WARPING_VERTICE_FORMAT #})
  poke p = poke (castPtr p :: Ptr {#type NV_GPU_WARPING_VERTICE_FORMAT #}) . fromEnum'


instance Storable NV_MONITOR_CONN_TYPE where
  sizeOf _ = {#sizeof NV_MONITOR_CONN_TYPE #}
  alignment _ = {#alignof NV_MONITOR_CONN_TYPE #}
  peek p = toEnum' <$> peek (castPtr p :: Ptr {#type NV_MONITOR_CONN_TYPE #})
  poke p = poke (castPtr p :: Ptr {#type NV_MONITOR_CONN_TYPE #}) . fromEnum'

data NvSBox = NvSBox
  { sX :: NvS32
  , sY :: NvS32
  , sWidth :: NvS32
  , sHeight :: NvS32
  } deriving (Eq, Show, Read)

instance Storable NvSBox where
  sizeOf _ = {#sizeof NvSBox #}
  alignment _ = {#alignof NvSBox #}
  peek p = NvSBox
    <$> coerce ({#get NvSBox->sX #} p)
    <*> coerce ({#get NvSBox->sY #} p)
    <*> coerce ({#get NvSBox->sWidth #} p)
    <*> coerce ({#get NvSBox->sHeight #} p)
  poke p NvSBox{..} = do
    {#set NvSBox.sX #} p $ coerce sX
    {#set NvSBox.sY #} p $ coerce sY
    {#set NvSBox.sWidth #} p $ coerce sWidth
    {#set NvSBox.sHeight #} p $ coerce sHeight

data NvScanoutInformation = NvScanoutInformation
  { sourceDesktopRect :: NvSBox
  , sourceViewportRect :: NvSBox
  , targetViewportRect :: NvSBox
  , targetDisplayWidth :: NvU32
  , targetDisplayHeight :: NvU32
  , cloneImportance :: NvU32
  , sourceToTargetRotation :: NV_ROTATE
  } deriving (Eq, Show, Read)

instance Storable NvScanoutInformation where
  sizeOf _ = {#sizeof NV_SCANOUT_INFORMATION #}
  alignment _ = {#alignof NV_SCANOUT_INFORMATION #}
  peek p = NvScanoutInformation
    <$> peekByteOff p {#offsetof NV_SCANOUT_INFORMATION->sourceDesktopRect #}
    <*> peekByteOff p {#offsetof NV_SCANOUT_INFORMATION->sourceViewportRect #}
    <*> peekByteOff p {#offsetof NV_SCANOUT_INFORMATION->targetViewportRect #}
    <*> peekByteOff p {#offsetof NV_SCANOUT_INFORMATION->targetDisplayWidth #}
    <*> peekByteOff p {#offsetof NV_SCANOUT_INFORMATION->targetDisplayHeight #}
    <*> peekByteOff p {#offsetof NV_SCANOUT_INFORMATION->cloneImportance #}
    <*> peekByteOff p {#offsetof NV_SCANOUT_INFORMATION->sourceToTargetRotation #}
  poke p NvScanoutInformation{..} = do
    pokeByteOff p {#offsetof NV_SCANOUT_INFORMATION->version #} _NV_SCANOUT_INFORMATION_VER
    pokeByteOff p {#offsetof NV_SCANOUT_INFORMATION->sourceDesktopRect #} sourceDesktopRect
    pokeByteOff p {#offsetof NV_SCANOUT_INFORMATION->sourceViewportRect #} sourceViewportRect
    pokeByteOff p {#offsetof NV_SCANOUT_INFORMATION->targetViewportRect #} targetViewportRect
    pokeByteOff p {#offsetof NV_SCANOUT_INFORMATION->targetDisplayWidth #} targetDisplayWidth
    pokeByteOff p {#offsetof NV_SCANOUT_INFORMATION->targetDisplayHeight #} targetDisplayHeight
    pokeByteOff p {#offsetof NV_SCANOUT_INFORMATION->cloneImportance #} cloneImportance
    pokeByteOff p {#offsetof NV_SCANOUT_INFORMATION->sourceToTargetRotation #}  sourceToTargetRotation

set_NV_SCANOUT_INFORMATION_VER :: Ptr NvScanoutInformation -> IO ()
set_NV_SCANOUT_INFORMATION_VER p = pokeByteOff p {#offsetof NV_SCANOUT_INFORMATION->version #} _NV_SCANOUT_INFORMATION_VER



newtype NvScanoutIntensityStateData = NvScanoutIntensityStateData
  { bEnabled'NvScanoutIntensityStateData :: NvU32
  } deriving (Eq, Show, Read)

instance Storable NvScanoutIntensityStateData where
  sizeOf _ = {#sizeof NV_SCANOUT_INTENSITY_STATE_DATA #}
  alignment _ = {#alignof NV_SCANOUT_INTENSITY_STATE_DATA #}
  peek p = NvScanoutIntensityStateData
    <$> coerce ({#get NV_SCANOUT_INTENSITY_STATE_DATA->bEnabled #} p)
  poke p NvScanoutIntensityStateData{..} = do
    pokeByteOff p {#offsetof NV_SCANOUT_INTENSITY_STATE_DATA->version #} _NV_SCANOUT_INTENSITY_STATE_VER
    pokeByteOff p {#offsetof NV_SCANOUT_INTENSITY_STATE_DATA->bEnabled #} bEnabled'NvScanoutIntensityStateData


newtype NvScanoutWarpingStateData = NvScanoutWarpingStateData
  { bEnabled'NvScanoutWarpingStateData :: NvU32
  } deriving (Eq, Show, Read)

instance Storable NvScanoutWarpingStateData where
  sizeOf _ = {#sizeof NV_SCANOUT_WARPING_STATE_DATA #}
  alignment _ = {#alignof NV_SCANOUT_WARPING_STATE_DATA #}
  peek p = NvScanoutWarpingStateData
    <$> coerce ({#get NV_SCANOUT_WARPING_STATE_DATA->bEnabled #} p)
  poke p NvScanoutWarpingStateData{..} = do
    pokeByteOff p {#offsetof NV_SCANOUT_WARPING_STATE_DATA->version #} _NV_SCANOUT_WARPING_STATE_VER
    pokeByteOff p {#offsetof NV_SCANOUT_WARPING_STATE_DATA->bEnabled #} bEnabled'NvScanoutWarpingStateData


data NvScanoutIntensityData = NvScanoutIntensityData
  { width :: NvU32
  , height :: NvU32
  , blendingTexture :: [Float]
  , offsetTexture :: [Float]
  , offsetTexChannels :: NvU32
  } deriving (Eq, Show, Read)

withNvScanoutIntensityData :: NvScanoutIntensityData -> (Ptr NvScanoutIntensityData -> IO a) -> IO a
withNvScanoutIntensityData NvScanoutIntensityData{..} f
  = allocaBytes {#sizeof NV_SCANOUT_INTENSITY_DATA #} $ \p ->
    withArray (map (realToFrac :: Float -> CFloat) blendingTexture) $ \bt ->
    withArray (map (realToFrac :: Float -> CFloat) offsetTexture) $ \ot -> do
      pokeByteOff p {#offsetof NV_SCANOUT_INTENSITY_DATA->version #} _NV_SCANOUT_INTENSITY_DATA_VER
      pokeByteOff p {#offsetof NV_SCANOUT_INTENSITY_DATA->width #} width
      pokeByteOff p {#offsetof NV_SCANOUT_INTENSITY_DATA->height #} height
      pokeByteOff p {#offsetof NV_SCANOUT_INTENSITY_DATA->blendingTexture #} (if null blendingTexture then nullPtr else bt)
      pokeByteOff p {#offsetof NV_SCANOUT_INTENSITY_DATA->offsetTexture #} (if null offsetTexture then nullPtr else ot)
      pokeByteOff p {#offsetof NV_SCANOUT_INTENSITY_DATA->offsetTexChannels #} offsetTexChannels
      f p

data NvScanoutWarpingData = NvScanoutWarpingData
  { vertices :: [Float]
  , vertexFormat :: NV_GPU_WARPING_VERTICE_FORMAT
  , numVertices :: Int
  , textureRect :: NvSBox
  } deriving (Eq, Show, Read)

withNvScanoutWarpingData :: NvScanoutWarpingData -> (Ptr NvScanoutWarpingData -> IO a) -> IO a
withNvScanoutWarpingData NvScanoutWarpingData{..} f
  = allocaBytes {#sizeof NV_SCANOUT_WARPING_DATA #} $ \p ->
    with textureRect $ \tr ->
    withArray (map (realToFrac :: Float -> CFloat) vertices) $ \vcs -> do
      pokeByteOff p {#offsetof NV_SCANOUT_WARPING_DATA->version #} _NV_SCANOUT_WARPING_VER
      pokeByteOff p {#offsetof NV_SCANOUT_WARPING_DATA->vertices #} vcs
      pokeByteOff p {#offsetof NV_SCANOUT_WARPING_DATA->vertexFormat #} vertexFormat
      pokeByteOff p {#offsetof NV_SCANOUT_WARPING_DATA->numVertices #} (fromIntegral numVertices :: CInt)
      pokeByteOff p {#offsetof NV_SCANOUT_WARPING_DATA->textureRect #} tr
      f p



data NvGPUDisplayIds = NvGPUDisplayIds
  { connectorType :: NV_MONITOR_CONN_TYPE
  , displayId :: NvDisplayId
  , isDynamic :: Bool
  , isMultiStreamRootNode :: Bool
  , isActive :: Bool
  , isCluster :: Bool
  , isOSVisible :: Bool
  , isWFD :: Bool
  , isConnected :: Bool
  , isPhysicallyConnected :: Bool
  } deriving (Eq, Show, Read)


instance Storable NvGPUDisplayIds where
  sizeOf _ = 16 -- {#sizeof NV_GPU_DISPLAYIDS #}
  alignment _ = 8 -- {#alignof NV_GPU_DISPLAYIDS #}
  peek p = do
    connectorType <- peekByteOff p 4 -- {#offsetof NV_GPU_DISPLAYIDS->connectorType #}
    displayId <- peekByteOff p 8 -- {#offsetof NV_GPU_DISPLAYIDS->displayId #}
    bf <- peekByteOff p 12 -- ({#offsetof NV_GPU_DISPLAYIDS->displayId #} + {#sizeof NvU32 #})
    let isDynamic = testBit (bf :: NvU32) 0
        isMultiStreamRootNode = testBit bf 1
        isActive = testBit bf 2
        isCluster = testBit bf 3
        isOSVisible = testBit bf 4
        isWFD = testBit bf 5
        isConnected = testBit bf 6
        isPhysicallyConnected = testBit bf 17
    return NvGPUDisplayIds {..}
  poke p NvGPUDisplayIds{..} = do
      pokeByteOff p 0 _NV_GPU_DISPLAYIDS_VER -- {#offsetof NV_GPU_DISPLAYIDS->version #} _NV_GPU_DISPLAYIDS_VER
      pokeByteOff p 4 connectorType -- {#offsetof NV_GPU_DISPLAYIDS->connectorType #} connectorType
      pokeByteOff p 8 displayId -- {#offsetof NV_GPU_DISPLAYIDS->displayId #} displayId
      pokeByteOff p 12 -- ({#offsetof NV_GPU_DISPLAYIDS->displayId #} + {#sizeof NvU32 #})
        $ f 0 isDynamic
        $ f 1 isMultiStreamRootNode
        $ f 2 isActive
        $ f 3 isCluster
        $ f 4 isOSVisible
        $ f 5 isWFD
        $ f 6 isConnected
        $ f 17 isPhysicallyConnected
        $ zeroBits
    where
      f :: Int -> Bool -> NvU32 -> NvU32
      f i True x = setBit x i
      f _ False x = x

set_NV_GPU_DISPLAYIDS_VER :: Ptr NvGPUDisplayIds -> IO ()
set_NV_GPU_DISPLAYIDS_VER p = pokeByteOff p {#offsetof NV_GPU_DISPLAYIDS->version #} _NV_GPU_DISPLAYIDS_VER


data NVVIOCONFIG



newtype NvU32 = NvU32 {#type NvU32#}
  deriving (Eq, Ord, Show, Read, Num, Real, Integral, Enum, Bounded, Storable, Bits)
newtype NvS32 = NvS32 {#type NvS32#}
  deriving (Eq, Ord, Show, Read, Num, Real, Integral, Enum, Bounded, Storable, Bits)

newtype NvDisplayId = NvDisplayId NvU32
  deriving (Eq, Ord, Show, Read, Storable)


{#fun unsafe NvAPI_Initialize as c_NvAPI_Initialize
  {} -> `()' checkError*#}

{#fun unsafe NvAPI_Unload as c_NvAPI_Unload
  {} -> `()' checkError*#}

{#fun unsafe NvAPI_EnumPhysicalGPUs as c_NvAPI_EnumPhysicalGPUs
  {id `Ptr NvPhysicalGpuHandle', castPtr `Ptr NvU32' } -> `()' checkError*#}

{#fun unsafe NvAPI_EnumNvidiaDisplayHandle as c_NvAPI_EnumNvidiaDisplayHandle
  {coerce `NvU32', castPtr `Ptr NvDisplayHandle'} -> `NvAPIStatus' toEnum' #}

{#fun unsafe NvAPI_GetAssociatedNvidiaDisplayName as c_NvAPI_GetAssociatedNvidiaDisplayName
  {`NvDisplayHandle', id `CString'} -> `()' checkError*#}

{#fun unsafe NvAPI_GetAssociatedNvidiaDisplayHandle as c_NvAPI_GetAssociatedNvidiaDisplayHandle
  {id `CString', id `Ptr NvDisplayHandle'} -> `()' checkError*#}

{#fun unsafe NvAPI_GetInterfaceVersionString as c_NvAPI_GetInterfaceVersionString
  {id `CString'} -> `()' checkError*#}


{#fun unsafe NvAPI_DISP_GetDisplayIdByDisplayName as c_NvAPI_DISP_GetDisplayIdByDisplayName
  {id `CString', castPtr `Ptr NvDisplayId'} -> `()' checkError*#}

{#fun unsafe NvAPI_GPU_GetConnectedDisplayIds as c_NvAPI_GPU_GetConnectedDisplayIds
  {id `NvPhysicalGpuHandle', castPtr `Ptr NvGPUDisplayIds', castPtr `Ptr NvU32', coerce `NvU32'} -> `()' checkError*#}


{#fun NvAPI_GPU_GetScanoutConfiguration as c_NvAPI_GPU_GetScanoutConfiguration
  {coerce `NvDisplayId', castPtr `Ptr NvSBox', castPtr `Ptr NvSBox'} -> `()' checkError*#}

{#fun NvAPI_GPU_GetScanoutConfigurationEx as c_NvAPI_GPU_GetScanoutConfigurationEx
  {coerce `NvDisplayId', castPtr `Ptr NvScanoutInformation'} -> `()' checkError*#}

{#fun unsafe NvAPI_GPU_GetScanoutIntensityState as c_NvAPI_GPU_GetScanoutIntensityState
  {coerce `NvDisplayId', castPtr `Ptr NvScanoutIntensityStateData'} -> `()' checkError*#}

{#fun unsafe NvAPI_GPU_GetScanoutWarpingState as c_NvAPI_GPU_GetScanoutWarpingState
  {coerce `NvDisplayId', castPtr `Ptr  NvScanoutWarpingStateData'} -> `()' checkError*#}


{#fun unsafe NvAPI_GPU_SetScanoutCompositionParameter as c_NvAPI_GPU_SetScanoutCompositionParameter
  { coerce `NvDisplayId'
  , fromEnum' `NV_GPU_SCANOUT_COMPOSITION_PARAMETER'
  , fromEnum' `NV_GPU_SCANOUT_COMPOSITION_PARAMETER_VALUE'
  , castPtr `Ptr CFloat' } -> `()' checkError*#}

{#fun unsafe NvAPI_GPU_SetScanoutIntensity as c_NvAPI_GPU_SetScanoutIntensity
  { coerce `NvDisplayId'
  , castPtr `Ptr NvScanoutIntensityData'
  , castPtr `Ptr CInt' } -> `()' checkError*#}


{#fun unsafe NvAPI_GPU_SetScanoutWarping as c_NvAPI_GPU_SetScanoutWarping
  { coerce `NvDisplayId'
  , castPtr `Ptr NvScanoutWarpingData'
  , castPtr `Ptr CInt'
  , castPtr `Ptr CInt' } -> `()' checkError*#}

{#fun unsafe NvAPI_GPU_WorkstationFeatureQuery as c_NvAPI_GPU_WorkstationFeatureQuery
  { id `NvPhysicalGpuHandle'
  , coerce `Ptr NvU32'
  , coerce `Ptr NvU32'
  } -> `()' checkError*#}


{#fun unsafe NvAPI_VIO_SetConfig as c_NvAPI_VIO_SetConfig
  { id `NvVioHandle'
  , castPtr `Ptr NVVIOCONFIG'
  } -> `()' checkError*#}


{#fun unsafe NvAPI_VIO_GetConfig as c_NvAPI_VIO_GetConfig
  { id `NvVioHandle'
  , castPtr `Ptr NVVIOCONFIG'
  } -> `()' checkError*#}




toEnum' :: (Integral a, Enum b) => a -> b
toEnum' = toEnum . fromIntegral

fromEnum' :: (Num a, Enum b) => b -> a
fromEnum' = fromIntegral . fromEnum

errString :: ForeignPtr CChar
errString = unsafePerformIO $ mallocForeignPtrBytes _NVAPI_SHORT_STRING_MAX

foreign import ccall unsafe "NvAPI_GetErrorMessage"
  c'getErrorMessage :: {#type NvAPI_Status#} -> CString -> IO ()

checkError :: {#type NvAPI_Status#} -> IO ()
checkError ec
    | status == NVAPI_OK = return ()
    | otherwise = withForeignPtr errString $ \sPtr -> do
        c'getErrorMessage ec sPtr
        err <- peekCString sPtr
        ioError $
          mkIOError illegalOperationErrorType 
              ("NvAPI call failed (" ++ show status ++ "): " ++ err)
              Nothing Nothing
  where
    status = toEnum' ec

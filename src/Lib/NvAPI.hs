{-# LANGUAGE FlexibleContexts #-}
module Lib.NvAPI
  ( findAllDisplays
  , initNvAPI
  , destroyNvAPI
  , createNewConfig
  , applyDisplayIllumination
  , applyDisplayIntensity
  , resetDisplayIntensity
  , DisplayEnv

  , enumDisplayHandles
  , displayHandleToName
  , displayNameToHandle
  , enumPhysicalGPUs
  , getConnectedDisplays
  , getDisplayRects
  )
where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader.Class
import           Data.List                  (sortBy)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Foreign.C.String           (peekCString, withCString)
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable

import           Lib.Dxva.FFI               as Dxva
import           Lib.Mosaic
import           Lib.NvAPI.FFI              as NvAPI



type DisplayEnv = Map DisplayName (MonitorDesc, NvDisplayId)

findAllDisplays :: IO DisplayEnv
findAllDisplays = do
  mons <- enumMonitors
  monsdIds <- forM mons $ \md -> do
    let na = szDevice $ winMoInfo md
    dId <- displayNameToId na
    return (DisplayName na, (md, dId))
  return $ Map.fromList monsdIds


initNvAPI :: MonadIO m => m ()
initNvAPI = liftIO c_NvAPI_Initialize

destroyNvAPI :: MonadIO m => m ()
destroyNvAPI = liftIO c_NvAPI_Unload


-- | Call nvapi and find out displays
createNewConfig :: (MonadReader DisplayEnv m, MonadIO m) => Int -> Int -> m Mosaic
createNewConfig nCols nRows = do
    dmap <- ask
    liftIO $ Mosaic <$> traverse (f . fst) dmap
  where
    tryBrighness :: [PMonitor] -> IO Val
    tryBrighness [] = pure 50
    tryBrighness (h:hs) = do
      ecb <- try (getMonitorBrightness (pmHandle h)) :: IO (Either SomeException MonBrightness)
      case ecb of
        Left err -> print err >> tryBrighness hs
        Right cb -> return $
          let minb = fromIntegral $ pdwMinimum cb :: Double
              maxb = fromIntegral $ pdwMaximum cb :: Double
              curb = fromIntegral $ pdwCurrent cb
          in  round $ (curb - minb) * 100 / (maxb - minb)

    f :: MonitorDesc -> IO Display
    f monDesc = do
      br <- tryBrighness $ phHandles monDesc
      return $ newDisplay (DisplayName . szDevice $ winMoInfo monDesc) br (Pos nCols nRows)


-- | Apply display settings
applyDisplayIllumination :: (MonadReader DisplayEnv m, MonadIO m) => Display -> m ()
applyDisplayIllumination d = do
  mmi <- fmap fst . Map.lookup (displayName d) <$> ask
  liftIO $ case mmi of
    Nothing -> putStrLn $ "Error: Display " <> getDisplayName (displayName d) <> " not found"
    Just mi -> case pmHandle <$> phHandles mi of
      [] -> putStrLn $ "Error: No monitors support changing brightness for " <> getDisplayName (displayName d)
      hs -> forM_ hs $ \h -> (do
        cb <- getMonitorBrightness  h
        let x :: Double
            x = realToFrac (illumination d) * fromIntegral (pdwMaximum cb - pdwMinimum cb)  / 100
                + fromIntegral (pdwMinimum cb)
        setMonitorBrightness h $ round x
        ) `catch` ( \e -> print (e :: SomeException) )



applyDisplayIntensity :: (MonadReader DisplayEnv m, MonadIO m) => Display -> m ()
applyDisplayIntensity d = do
  mdId <- fmap snd . Map.lookup (displayName d) <$> ask
  liftIO $ case mdId of
    Nothing -> putStrLn $ "Error: Display " <> getDisplayName (displayName d) <> " not found"
    Just dI -> withNvScanoutIntensityData siData
      (\siPtr -> alloca $ \sPtr ->
                  c_NvAPI_GPU_SetScanoutIntensity dI siPtr sPtr
      ) `catch` ( \e -> print (e :: SomeException) )
  where
    screenOrdering (Pos ix jx, _) (Pos iy jy, _)
      = compare ix iy <> compare jx jy
    scrs = map (screenRGB . snd) . sortBy screenOrdering $ Map.toList (screens d)
    tr2list (a,b,c) = map (\x -> realToFrac x / 100) [a,b,c]
    siData = NvScanoutIntensityData
        { width             = 2
        , height            = 2
        , blendingTexture   = scrs >>= tr2list . scale
        , offsetTexture     = scrs >>= tr2list . offset
        , offsetTexChannels = 3
        }


resetDisplayIntensity :: (MonadReader DisplayEnv m, MonadIO m) => Display -> m ()
resetDisplayIntensity d = do
  mdId <- fmap snd . Map.lookup (displayName d) <$> ask
  liftIO $ case mdId of
    Nothing -> putStrLn $ "Error: Display " <> getDisplayName (displayName d) <> " not found"
    Just dI ->
      withNvScanoutIntensityData NvScanoutIntensityData
                { width             = 1
                , height            = 1
                , blendingTexture   = []
                , offsetTexture     = []
                , offsetTexChannels = 3
                }
      (\siPtr -> alloca $ \sPtr ->
                  c_NvAPI_GPU_SetScanoutIntensity dI siPtr sPtr
      ) `catch` ( \e -> print (e :: SomeException) )



enumDisplayHandles :: IO [NvDisplayHandle]
enumDisplayHandles = alloca $ \dPtr -> go 0 dPtr
  where
    go i p = do
      s <- c_NvAPI_EnumNvidiaDisplayHandle i p
      case s of
        NVAPI_END_ENUMERATION -> pure []
        NVAPI_OK -> (:) <$> peek p <*> go (i+1) p
        _ -> do
          putStrLn $ "Warning: NvAPI_EnumNvidiaDisplayHandle status " <> show s <> " on iteration " <> show i
          if i >= 100
          then return []
          else go (i+1) p

displayHandleToName :: NvDisplayHandle -> IO String
displayHandleToName dh = allocaBytes  _NVAPI_SHORT_STRING_MAX $ \sPtr -> do
  c_NvAPI_GetAssociatedNvidiaDisplayName dh sPtr
  peekCString sPtr

displayNameToHandle :: String -> IO NvDisplayHandle
displayNameToHandle s = withCString s $ \sPtr -> alloca $ \dhPtr -> do
  c_NvAPI_GetAssociatedNvidiaDisplayHandle sPtr dhPtr
  peek dhPtr

displayNameToId :: String -> IO NvDisplayId
displayNameToId s = withCString s $ \sPtr -> alloca $ \diPtr -> do
  c_NvAPI_DISP_GetDisplayIdByDisplayName sPtr diPtr
  peek diPtr

enumPhysicalGPUs :: IO [NvPhysicalGpuHandle]
enumPhysicalGPUs = allocaArray _NVAPI_MAX_PHYSICAL_GPUS $ \pgpus ->
  alloca $ \pcount -> do
    c_NvAPI_EnumPhysicalGPUs pgpus pcount
    count <- peek pcount
    peekArray (fromIntegral count) pgpus

getConnectedDisplays :: NvPhysicalGpuHandle -> IO [NvGPUDisplayIds]
getConnectedDisplays gpuHandle = alloca $ \nPtr -> do
  c_NvAPI_GPU_GetConnectedDisplayIds gpuHandle nullPtr nPtr 0
  nDisplays <- peek nPtr
  if nDisplays == 0
    then return []
    else withArray (replicate (fromIntegral nDisplays)
                      NvGPUDisplayIds
                      { connectorType = NV_MONITOR_CONN_TYPE_UNINITIALIZED
                      , NvAPI.displayId = NvDisplayId 0
                      , isDynamic = False
                      , isMultiStreamRootNode = False
                      , isActive = False
                      , isCluster = False
                      , isOSVisible = False
                      , isWFD = False
                      , isConnected = True
                      , isPhysicallyConnected = True
                      }
                   ) $ \dsPtr -> do
      c_NvAPI_GPU_GetConnectedDisplayIds gpuHandle dsPtr nPtr 0
      filter isActive <$> peekArray (fromIntegral nDisplays) dsPtr

-- | return (desktopRect, scanoutRect, viewportRect)
getDisplayRects :: NvDisplayId -> IO (NvSBox, NvSBox, NvSBox)
getDisplayRects i = do
  (desktopRect, scanoutRect') <- alloca $ \drPtr -> alloca $ \srPtr -> do
    c_NvAPI_GPU_GetScanoutConfiguration i drPtr srPtr
    (,) <$> peek drPtr <*> peek srPtr

  escanoutConfiguration <- try $ alloca $ \siPtr -> do
    set_NV_SCANOUT_INFORMATION_VER siPtr
    c_NvAPI_GPU_GetScanoutConfigurationEx i siPtr
    peek siPtr
  (scanoutRect, viewportRect) <-
    case escanoutConfiguration :: Either IOError NvScanoutInformation of
      Left _ -> (scanoutRect', scanoutRect') <$ putStrLn
        "NvAPI_GPU_GetScanoutConfigurationEx is not supported. Ingoring"
      Right sc -> return
        ( scanoutRect' { sWidth  = fromIntegral $ targetDisplayWidth sc
                       , sHeight = fromIntegral $ targetDisplayHeight sc
                       }
        , targetViewportRect sc
        )

  return (desktopRect, scanoutRect, viewportRect)

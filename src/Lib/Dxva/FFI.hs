{-# LANGUAGE EmptyDataDecls #-}
module Lib.Dxva.FFI where

import           Control.Monad
import           Foreign.C.Types
import           Foreign.C.String
import           Foreign.Ptr
import           Foreign.Marshal.Utils          ( copyBytes, with )
import           Foreign.Marshal.Array
import           Foreign.Marshal.Alloc          ( alloca
                                                , allocaBytes
                                                )
import           Foreign.Storable
import           Data.Bits
import           Data.List                      ( dropWhile
                                                , dropWhileEnd
                                                )
import           Data.Char                      ( isSpace
                                                , isPrint
                                                )


data DC
data RECT

data WinMonitor
type WinMonitorHandle = Ptr WinMonitor

data PhysicalMonitor
type PhysicalMonitorHandle = Ptr PhysicalMonitor
data PMonitor = PMonitor
  { pmHandle :: PhysicalMonitorHandle
  , pmName :: String
  } deriving (Eq, Show)

instance Storable PMonitor where
  sizeOf _ = sizeOf (undefined :: PhysicalMonitorHandle)
           + (128 * sizeOf (undefined :: CWchar)) -- #define PHYSICAL_MONITOR_DESCRIPTION_SIZE 128
  alignment _ = alignment (undefined :: PhysicalMonitorHandle)
  peek p = PMonitor
    <$> peekByteOff p 0
    <*> peekCWStringMaxLen 128 (plusPtr p (sizeOf (undefined :: PhysicalMonitorHandle)))
  poke p (PMonitor h s) = withCWStringLen s $ \(sp, l) -> do
    pokeByteOff p 0 h
    copyBytes (plusPtr p (sizeOf (undefined :: PhysicalMonitorHandle))) sp (l * sizeOf (undefined :: CWchar))


peekCStringMaxLen :: Int -> CString -> IO String
peekCStringMaxLen n cs
  = dropWhileEnd (\c -> not (isPrint c) || c == '\0' || isSpace c ) <$> peekCStringLen (cs, n)

peekCWStringMaxLen :: Int -> CWString -> IO String
peekCWStringMaxLen n cs
  = dropWhileEnd (\c -> not (isPrint c) || c == '\0' || isSpace c ) <$> peekCWStringLen (cs, n)

data MonsLen = MonsLen
  { mlmons :: Ptr WinMonitorHandle
  , mllen :: Int
  }

instance Storable MonsLen where
  sizeOf    _ = sizeOf (undefined :: Ptr WinMonitorHandle) + sizeOf (undefined :: Ptr Int)
  alignment _ = max (alignment (undefined :: Ptr WinMonitorHandle)) (alignment (undefined :: Ptr Int))
  peek p = MonsLen <$> peekByteOff p 0 <*> peekByteOff p (sizeOf (undefined :: Ptr WinMonitorHandle))
  poke p (MonsLen h l) = do
    pokeByteOff p 0 h
    pokeByteOff p (sizeOf (undefined :: Ptr WinMonitorHandle)) l

type MONITORENUMPROC = WinMonitorHandle -> Ptr DC -> Ptr RECT -> Ptr MonsLen -> IO CInt
foreign import ccall safe "wrapper"
  wrapMONITORENUMPROC :: MONITORENUMPROC -> IO (FunPtr MONITORENUMPROC)


foreign import ccall safe "EnumDisplayMonitors"
  c_EnumDisplayMonitors :: Ptr DC -> Ptr RECT -> FunPtr MONITORENUMPROC -> Ptr MonsLen-> IO CInt

foreign import ccall unsafe "GetNumberOfPhysicalMonitorsFromHMONITOR"
  c_GetNumberOfPhysicalMonitorsFromHMONITOR :: WinMonitorHandle -> Ptr CULong -> IO CInt

foreign import ccall unsafe "GetPhysicalMonitorsFromHMONITOR"
  c_GetPhysicalMonitorsFromHMONITOR :: WinMonitorHandle -> CULong -> Ptr PMonitor -> IO CInt

foreign import ccall unsafe "GetMonitorInfoA"
  c_GetMonitorInfoA :: WinMonitorHandle -> Ptr WinMonitorInfo -> IO CInt


data WinRect = WinRect
  { wrLeft :: CLong
  , wrTop :: CLong
  , wrRight :: CLong
  , wrBottom :: CLong
  } deriving (Eq, Show, Read)

data WinMonitorInfo = WinMonitorInfo
  { rcMonitor :: WinRect
  , rcWork :: WinRect
  , dwFlags :: CULong
  , szDevice :: String -- (cbSize :: CULong === sizeOf WinMonitorInfo); CCHDEVICENAME = 32
  } deriving (Eq, Show, Read)

instance Storable WinRect where
  sizeOf    _ = sizeOf (undefined :: CLong) * 4
  alignment _ = alignment (undefined :: CLong)
  peek p = WinRect
        <$> peekByteOff p 0
        <*> peekByteOff p x
        <*> peekByteOff p (x * 2)
        <*> peekByteOff p (x * 3)
    where
      x = sizeOf (undefined :: CLong)
  poke p (WinRect l t r b) = do
      pokeByteOff p 0 l
      pokeByteOff p x t
      pokeByteOff p (x * 2) r
      pokeByteOff p (x * 3) b
    where
      x = sizeOf (undefined :: CLong)

wmiSizes :: [Int]
wmiSizes =
  [ sizeOf (undefined :: CULong)  -- DWORD   cbSize;
  , sizeOf (undefined :: WinRect) -- RECT    rcMonitor;
  , sizeOf (undefined :: WinRect) -- RECT    rcWork;
  , sizeOf (undefined :: CULong)  -- DWORD   dwFlags;
  , 32                            -- CHAR    szDevice[CCHDEVICENAME];
  ]

instance Storable WinMonitorInfo where
  sizeOf    _ = sum wmiSizes
  alignment _ = alignment (undefined :: CLong)
  peek p = WinMonitorInfo
      <$>  peekByteOff p (wmiOffs !! 1)
      <*>  peekByteOff p (wmiOffs !! 2)
      <*>  peekByteOff p (wmiOffs !! 3)
      <*>  peekCStringMaxLen 32 (plusPtr p (wmiOffs !! 4))
    where
      wmiOffs = scanl (+) 0 wmiSizes
  poke p w@(WinMonitorInfo a b c d) = do
      pokeByteOff p (wmiOffs !! 0) (sizeOf w)
      pokeByteOff p (wmiOffs !! 1) a
      pokeByteOff p (wmiOffs !! 2) b
      pokeByteOff p (wmiOffs !! 3) c
      unless (null d) $ withCStringLen d $ \(sp, l) -> 
        copyBytes (plusPtr p $ wmiOffs !! 4) sp (l * sizeOf (undefined :: CChar))
    where
      wmiOffs = scanl (+) 0 wmiSizes

data MonitorDesc = MonitorDesc
  { winHandle :: WinMonitorHandle
  , winMoInfo :: WinMonitorInfo
  , phHandles :: [PMonitor]
  } deriving (Eq, Show)

enumMonitors :: IO [MonitorDesc]
enumMonitors = do
  efun <- wrapMONITORENUMPROC $ \inh _ _ outh -> do
    MonsLen mPtr n <- peek outh
    pokeElemOff mPtr n inh
    poke outh $ MonsLen mPtr $ n + 1
    return 1
  mons <- allocaArray 1024 $ \arrPtr -> alloca $ \outh -> do
    poke outh (MonsLen arrPtr 0)
    assertingTrue "EnumDisplayMonitors"
      $ c_EnumDisplayMonitors nullPtr nullPtr efun outh
    n <- mllen <$> peek outh
    peekArray n arrPtr
  freeHaskellFunPtr efun

  -- get physical mons
  forM mons $ \mh -> alloca $ \nPtr -> do
    assertingTrue "GetNumberOfPhysicalMonitorsFromHMONITOR"
      $ c_GetNumberOfPhysicalMonitorsFromHMONITOR mh nPtr
    n <- peek nPtr
    pms <- allocaArray (fromIntegral n) $ \pmhPtr -> do
      assertingTrue "GetPhysicalMonitorsFromHMONITOR"
        $ c_GetPhysicalMonitorsFromHMONITOR mh n pmhPtr
      peekArray (fromIntegral n) pmhPtr
    MonitorDesc mh <$> getMonitorInfo mh <*> pure pms


getMonitorInfo :: WinMonitorHandle -> IO WinMonitorInfo
getMonitorInfo h = with
    WinMonitorInfo
    { rcMonitor = defr
    , rcWork = defr
    , dwFlags = 0
    , szDevice = []
    } $ \wmiPtr -> do
        assertingTrue "GetMonitorInfoA"
          $ c_GetMonitorInfoA h wmiPtr
        peek wmiPtr
  where
    defr = WinRect 0 0 0 0

foreign import ccall unsafe "GetMonitorBrightness"
  c_GetMonitorBrightness :: PhysicalMonitorHandle -> Ptr CULong -> Ptr CULong -> Ptr CULong -> IO CInt

foreign import ccall unsafe "SetMonitorBrightness"
  c_SetMonitorBrightness :: PhysicalMonitorHandle -> CULong -> IO CInt




data MonBrightness = MonBrightness
  { pdwMinimum :: CULong
  , pdwCurrent :: CULong
  , pdwMaximum :: CULong
  } deriving (Eq, Show, Read)

getMonitorBrightness :: PhysicalMonitorHandle -> IO MonBrightness
getMonitorBrightness h = alloca $ \pmi -> alloca $ \pcu -> alloca $ \pma -> do
  assertingTrue "GetMonitorBrightness" $ c_GetMonitorBrightness h pmi pcu pma
  MonBrightness <$> peek pmi <*> peek pcu <*> peek pma

setMonitorBrightness :: PhysicalMonitorHandle -> CULong -> IO ()
setMonitorBrightness h =
  assertingTrue "SetMonitorBrightness" . c_SetMonitorBrightness h













foreign import ccall unsafe "GetLastError"
  c_GetLastError :: IO CULong

foreign import ccall unsafe "FormatMessageA"
  c_FormatMessage :: CULong -> Ptr () -> CULong -> CULong -> CString -> CULong -> Ptr () -> IO CULong

getErrorMessage :: CULong -> IO String
getErrorMessage ec = allocaBytes max_length $ \strPtr -> do
  len <- c_FormatMessage (0x00001000 .|. 0x00000200) -- dwFlags: FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS
                         nullPtr  -- lpSource
                         ec -- dwMessageId
                         (shiftL 0x01 10 .|. 0x00) -- dwLanguageId: MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT)
                         strPtr -- buffer ptr
                         (fromIntegral max_length) -- nSize
                         nullPtr -- Arguments
  stripSpaces <$> peekCStringLen (strPtr, fromIntegral len)
 where
  max_length  = 4096
  stripSpaces = dropWhileEnd isSpace . dropWhile isSpace

fireLastError :: String -> IO a
fireLastError fname = do
  ec  <- c_GetLastError
  msg <- getErrorMessage ec
  fail $ fname <> " failed (" <> show ec <> "): " <> msg

assertingTrue :: String -> IO CInt -> IO ()
assertingTrue fname m = m >>= \s -> when (s == 0) (fireLastError fname)

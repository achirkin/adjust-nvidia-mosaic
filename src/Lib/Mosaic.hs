{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving   #-}
module Lib.Mosaic
  ( Mosaic (..), Display (..), Screen (..), Transform (..), Pos (..), RGB, Intensity
  , UpdateTransform (..), UpdateType (..), UpdateChannel (..), DisplayName (..), Val (..)
  , newDisplay, updateDisplay, updateScreen, updateIllumination, screenRGB
  ) where

import qualified Data.Aeson               as JSON
import           Data.Aeson.Types
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import           Text.Read (readPrec)
import           GHC.Generics (Generic)

newtype DisplayName = DisplayName { getDisplayName :: String }
  deriving (Eq, Ord, FromJSON, ToJSON)

instance Show DisplayName where
  show (DisplayName s) = show s

instance Read DisplayName where
  readPrec = DisplayName <$> readPrec


data UpdateType = UpdateScale | UpdateOffset
  deriving (Eq, Ord, Show, Read)
data UpdateChannel = UpdateIntensity | UpdateR | UpdateG | UpdateB
  deriving (Eq, Ord, Show, Read)


class UpdateTransform a where
  updateTransform :: UpdateType -> UpdateChannel -> (Val -> Val) -> a -> a

instance UpdateTransform Screen where
  updateTransform UpdateScale  UpdateIntensity f (Intensity (Transform s o))
    = Intensity $ Transform (f s) o
  updateTransform UpdateScale  UpdateIntensity f (RGB (Transform (sr, sg, sb) o))
    = RGB $ Transform (f sr, f sg, f sb) o
  updateTransform UpdateScale  UpdateR         f (Intensity (Transform s o))
    = RGB $ Transform (f s, s, s) (o, o, o)
  updateTransform UpdateScale  UpdateR         f (RGB (Transform (sr, sg, sb) o))
    = RGB $ Transform (f sr, sg, sb) o
  updateTransform UpdateScale  UpdateG         f (Intensity (Transform s o))
    = RGB $ Transform (s, f s, s) (o, o, o)
  updateTransform UpdateScale  UpdateG         f (RGB (Transform (sr, sg, sb) o))
    = RGB $ Transform (sr, f sg, sb) o
  updateTransform UpdateScale  UpdateB         f (Intensity (Transform s o))
    = RGB $ Transform (s, s, f s) (o, o, o)
  updateTransform UpdateScale  UpdateB         f (RGB (Transform (sr, sg, sb) o))
    = RGB $ Transform (sr, sg, f sb) o

  updateTransform UpdateOffset UpdateIntensity f (Intensity (Transform o s))
    = Intensity $ Transform o (f s)
  updateTransform UpdateOffset UpdateIntensity f (RGB (Transform o (sr, sg, sb)))
    = RGB $ Transform o (f sr, f sg, f sb)
  updateTransform UpdateOffset UpdateR         f (Intensity (Transform o s))
    = RGB $ Transform (o, o, o) (f s, s, s)
  updateTransform UpdateOffset UpdateR         f (RGB (Transform o (sr, sg, sb)))
    = RGB $ Transform o (f sr, sg, sb)
  updateTransform UpdateOffset UpdateG         f (Intensity (Transform o s))
    = RGB $ Transform (o, o, o) (s, f s, s)
  updateTransform UpdateOffset UpdateG         f (RGB (Transform o (sr, sg, sb)))
    = RGB $ Transform o (sr, f sg, sb)
  updateTransform UpdateOffset UpdateB         f (Intensity (Transform o s))
    = RGB $ Transform (o, o, o) (s, s, f s)
  updateTransform UpdateOffset UpdateB         f (RGB (Transform o (sr, sg, sb)))
    = RGB $ Transform o (sr, sg, f sb)


instance UpdateTransform Display where
  updateTransform t c f d = d { screens = updateTransform t c f <$> screens d }

updateIllumination :: (Val -> Val) -> Display -> Display
updateIllumination f d = d { illumination = f $ illumination d }

-- | Create a new display with no transforms
newDisplay :: DisplayName -> Val -> Pos -> Display
newDisplay displayName illumination size = Display {..}
  where
    screens = Map.fromList
      [ (Pos i j, mempty)
        | i <- [1..col size], j <- [1..row size]
      ]

updateScreen :: Pos -> (Screen -> Screen) -> Display -> Display
updateScreen p f d = d { screens = Map.alter g p $ screens d }
  where
    g Nothing  = Just $ f mempty
    g (Just s) = Just $ f s


updateDisplay :: Monad f => DisplayName -> (Display -> f Display) -> Mosaic -> f Mosaic
updateDisplay p f m = (\ds -> m { displays = ds }) <$> Map.alterF g p (displays m)
  where
    g Nothing  = return Nothing
    g (Just d) = Just <$> f d

-- | Value from 0 to 100
newtype Val = Val Int
  deriving (Eq, Ord, FromJSON, ToJSON, Num, Enum, Real, Integral)

instance Show Val where
  show (Val i) = show i

instance Read Val where
  readPrec = Val <$> readPrec

  

type RGB = (Val, Val, Val)
type Intensity = Val

i2rgb :: Intensity -> RGB
i2rgb i = (i,i,i)

data Pos = Pos { col :: Int, row :: Int}
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON Pos
instance FromJSON Pos

instance ToJSONKey Pos where
  toJSONKey = toJSONKeyText $ \(Pos x y) -> T.pack ("pos-" ++ show x ++ "-" ++ show y)
instance FromJSONKey Pos where
  fromJSONKey = FromJSONKeyTextParser $ \s -> case T.breakOn "-" <$> T.stripPrefix "pos-" s of
    Nothing -> fail $ "Failed to parse position in format 'pos-XX-YY' (given " <> show s <> ")"
    Just (scol, srow) -> case Pos <$> JSON.decodeStrict (T.encodeUtf8 scol)
                                  <*> JSON.decodeStrict (T.encodeUtf8 $ T.drop 1 srow) of
      Nothing -> fail $ "Failed to parse position in format 'pos-XX-YY' (given " <> show s <> ")"
      Just v  -> return v


-- | Blending is defined by this formula:
--
--   f ( x ) = scale * x + offset
--
--   The values shall be in range from 0 to 1
data Transform a
  = Transform
    { scale  :: a
    , offset :: a
    }
  deriving (Eq, Ord, Show, Read, Generic)

i2rgbt :: Transform Intensity -> Transform RGB
i2rgbt (Transform s o) = Transform (i2rgb s) (i2rgb o)

instance ToJSON a => ToJSON (Transform a)
instance FromJSON a => FromJSON (Transform a)

instance Semigroup (Transform Intensity) where
  Transform s1 o1 <> Transform s2 o2 =
    Transform (div (s1 * s2) 100) (div (s2*o1 + o2) 100)

instance Monoid (Transform Intensity) where
  mempty = Transform 100 0

instance Semigroup (Transform RGB) where
  Transform (sr1, sg1, sb1) (or1, og1, ob1) <> Transform (sr2, sg2, sb2) (or2, og2, ob2) =
    Transform (div (sr1 * sr2) 100, div (sg1 * sg2) 100, div (sb1 * sb2) 100)
              (div (sr2*or1 + or2) 100, div (sg2*og1 + og2) 100, div (sb2*ob1 + ob2) 100)

instance Monoid (Transform RGB) where
  mempty = Transform (100, 100, 100) (0, 0, 0)

data Screen
  = RGB (Transform RGB)
    -- ^ Define blending per channel
  | Intensity (Transform Intensity)
    -- ^ Define a single blending for all 3 channels
  deriving (Eq, Ord, Show, Read, Generic)


screenRGB :: Screen -> Transform RGB
screenRGB (Intensity i) = i2rgbt i
screenRGB (RGB rgb) = rgb

instance Semigroup Screen where
  RGB t1 <> RGB t2 = RGB (t1 <> t2)
  Intensity t1 <> Intensity t2 = Intensity (t1 <> t2)
  RGB t1 <> Intensity t2 = RGB (t1 <> i2rgbt t2)
  Intensity t1 <> RGB t2 = RGB (i2rgbt t1 <> t2)

instance Monoid Screen where
  mempty = Intensity mempty

instance ToJSON Screen
instance FromJSON Screen

-- | A single element of a mosaic
data Display
  = Display
  { displayName  :: DisplayName
    -- ^ get id of display from nvapi
  , illumination :: Val
    -- ^ default illumination is also to be obtained from nvapi
  , size         :: Pos
    -- ^ Split the display into this many screens of equal size
  , screens      :: Map Pos Screen
    -- ^ Individual blend specifications per screen
  }
  deriving (Eq, Show, Read, Generic)

instance ToJSON Display
instance FromJSON Display

-- | A collection of displays
newtype Mosaic
  = Mosaic
  { displays :: Map DisplayName Display
  }
  deriving (Eq, Show, Read, Generic)

instance ToJSON Mosaic where
  toJSON = toJSON . Map.elems . displays
instance FromJSON Mosaic where
  parseJSON =
    fmap (Mosaic . Map.fromList . map ( \d -> (displayName d, d) )) . parseJSON

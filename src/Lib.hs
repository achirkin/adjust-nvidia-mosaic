{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
  ( interactive
  , UpdateType(..)
  , UpdateChannel(..)
  , newDisplay
  , updateDisplay
  )
where

import Control.Monad (when)
import           Control.Exception              ( bracket_ )
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Class
import qualified Data.ByteString.Char8         as BS
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Yaml
import           System.Console.Haskeline

import           Lib.Mosaic
import           Lib.NvAPI

-- v <- runInputT defaultSettings $ do
--   outputStrLn "Into how many columns shall one display be split?"
--   ncol <- getInputChar "display colnum% "
--   outputStrLn "Into how many rows shall one display be split?"
--   ncol <- getInputChar "display rownum% "
--   liftIO $ createNewConfig ncol nrow

interactive :: IO ()
interactive = do
  emo0 <- decodeFileEither configFileName
  bracket_ initNvAPI exitActions $ do
    env <- findAllDisplays
    flip runReaderT env $ do
      mo0 <- case emo0 of
        Left err -> do
          liftIO $ print err
          liftIO $ putStrLn "Creating a new config..."
          v <- createNewConfig 2 2
          liftIO $ putStrLn "Done."
          return v
        Right v -> do
          liftIO $ putStrLn "Found an existing config file; applying it..."
          mapM_ applyDisplayIntensity    (displays v)
          mapM_ applyDisplayIllumination (displays v)
          liftIO $ putStrLn "Done."
          return v
      when (Map.null $ displays mo0) $ fail "Could not find controllable displays!"
      let es0 = EditIllumination (head . Map.keys $ displays mo0)
      runInputT defaultSettings (loop es0 mo0)
 where
  configFileName = "adjust-nvidia-mosaic.yaml"
  exitActions :: IO ()
  exitActions = do
    destroyNvAPI
    putStrLn "Exiting!"
  loop :: EditState -> Mosaic -> InputT (ReaderT DisplayEnv IO) ()
  loop es mo =
    let
      commands = keyMap (Map.keys $ displays mo) es
      showCmd (c, kf) = '\t' : c : (" - " <> comment kf)
    in
      do
        mapM_ (outputStrLn . showCmd) $ Map.toList commands
        minput <- getInputChar "% "
        case minput of
          Nothing -> return ()
          Just input -> case Map.lookup input commands of
                Nothing ->
                  outputStrLn ("Unknown command: " ++ show input) >> loop es mo
                Just kf -> do
                  let es' = state kf
                  mo' <- do
                    newmo <- lift $ fun kf mo
                    liftIO $ encodeFile configFileName newmo
                    liftIO $ BS.putStrLn $ encode newmo
                    return newmo
                  outputStrLn $ showState es'
                  loop es' mo'


data KeyFun = KeyFun
  { comment :: String
  , state   :: EditState
  , fun     :: Mosaic -> ReaderT DisplayEnv IO Mosaic
  }

data EditLevel = EditDisplay | EditScreen SelectionState
  deriving (Eq, Show, Read)
data SelectionState = SelectedNone | SelectedCol Int | SelectedAll Pos
  deriving (Eq, Show, Read)
data EditState = EditState DisplayName EditLevel UpdateType UpdateChannel
               | EditIllumination DisplayName
  deriving (Eq, Show, Read)


showState :: EditState -> String
showState es = case es of
  EditIllumination ss ->
    "Editing display illumination;\n\tdisplay: " <> getDisplayName ss <> ".\n"
  EditState ss EditDisplay ut uc ->
    "Editing display;\n\tchannel: "
      <> selCh uc
      <> " ["
      <> selUt ut
      <> "];\n\tdisplay: "
      <> getDisplayName ss
      <> ".\n"
  EditState ss (EditScreen sss) ut uc ->
    "Editing screen;\n\tchannel: "
      <> selCh uc
      <> " ["
      <> selUt ut
      <> "];\n\tdisplay: "
      <> getDisplayName ss
      <> ";\n\tscreen: "
      <> selPos sss
      <> ".\n"
 where
  selPos SelectedNone            = "(?,?)"
  selPos (SelectedCol i        ) = "(" <> show i <> ",?)"
  selPos (SelectedAll (Pos i j)) = "(" <> show i <> "," <> show j <> ")"

  selUt UpdateOffset = "offset"
  selUt UpdateScale  = "scale"

  selCh UpdateIntensity = "all"
  selCh UpdateR         = "red"
  selCh UpdateG         = "green"
  selCh UpdateB         = "blue"



keyUpdateVal :: Bool -> EditState -> Maybe KeyFun
keyUpdateVal inc es = case es of
  EditIllumination sd ->
    Just $ KeyFun com es . updateDisplay sd $ \d ->
      let d' = updateIllumination f d in d' <$ applyDisplayIllumination d'

  EditState sd EditDisplay ut uc ->
    Just $ KeyFun com es . updateDisplay sd $ \d ->
      let d' = updateTransform ut uc f d in d' <$ applyDisplayIntensity d'

  EditState sd (EditScreen (SelectedAll ps)) ut uc ->
    Just $ KeyFun com es . updateDisplay sd $ \d ->
      let d' = updateScreen ps (updateTransform ut uc f) d
      in  d' <$ applyDisplayIntensity d'

  _ -> Nothing
 where
  com = if inc then "Increase value by 1%" else "Decrease value by 1%"
  f x = if inc then min 100 (x + 1) else max 0 (x - 1)

keySetIllumination :: EditState -> Maybe KeyFun
keySetIllumination es = case es of
  EditIllumination _ -> Just $ KeyFun com es pure
  EditState s _ _ _  -> Just $ KeyFun com (EditIllumination s) pure
  where com = "Update display illumination"

keySetUpdateType :: UpdateType -> EditState -> Maybe KeyFun
keySetUpdateType t' es = case es of
  EditIllumination _ -> Nothing
  EditState s l _ c  -> Just $ KeyFun com (EditState s l t' c) pure
 where
  com = case t' of
    UpdateScale  -> "Update value scale"
    UpdateOffset -> "Update value offset"

keySetUpdateChannel :: UpdateChannel -> EditState -> Maybe KeyFun
keySetUpdateChannel c' es = case es of
  EditIllumination s ->
    Just $ KeyFun com (EditState s EditDisplay UpdateScale c') pure
  EditState s l t _ -> Just $ KeyFun com (EditState s l t c') pure
 where
  com = case c' of
    UpdateIntensity -> "Update all channels"
    UpdateR         -> "Update red channel"
    UpdateG         -> "Update green channel"
    UpdateB         -> "Update blue channel"


keySetEditLevel :: EditLevel -> EditState -> Maybe KeyFun
keySetEditLevel l' es = case es of
  EditIllumination _ -> Nothing
  EditState s l t c  -> if l == l'
    then Nothing
    else Just $ KeyFun com (EditState s l' t c) pure
 where
  com = case l' of
    EditDisplay  -> "Select display"
    EditScreen _ -> "Select screen"


keyDigit :: [DisplayName] -> Int -> EditState -> Maybe KeyFun
keyDigit allDs i es = case es of
 
  EditIllumination _          | i > n -> Nothing
  EditState _ EditDisplay _ _ | i > n -> Nothing

  EditIllumination _ -> Just $ KeyFun (com EditDisplay SelectedNone)
                                      (EditIllumination newD)
                                      pure

  EditState _ EditDisplay t ch -> Just $ KeyFun
    (com EditDisplay SelectedNone)
    (EditState newD EditDisplay t ch)
    pure

  EditState s (EditScreen (SelectedCol j)) t ch -> Just $ KeyFun
    (com (EditScreen (SelectedCol j)) (SelectedCol j))
    (EditState s (EditScreen (SelectedAll (Pos j i))) t ch)
    pure

  EditState s (EditScreen _) t ch -> Just $ KeyFun
    (com (EditScreen SelectedNone) SelectedNone)
    (EditState s (EditScreen (SelectedCol i)) t ch)
    pure
 where
  n = length allDs
  newD = allDs !! (i - 1)
  com EditDisplay  _               = "Select display " <> getDisplayName newD
  com EditScreen{} (SelectedCol j) = "Select screen " <> show (j, i)
  com EditScreen{} _               = "Select screen column " <> show i


keyMap :: [DisplayName] -> EditState -> Map Char KeyFun
keyMap allDs es =
  Map.fromList
    $   [ ('+', keyUpdateVal True es)
        , ('-', keyUpdateVal False es)
        , ('i', keySetIllumination es)
        , ('c', keySetUpdateType UpdateScale es)
        , ('o', keySetUpdateType UpdateOffset es)
        , ('a', keySetUpdateChannel UpdateIntensity es)
        , ('r', keySetUpdateChannel UpdateR es)
        , ('g', keySetUpdateChannel UpdateG es)
        , ('b', keySetUpdateChannel UpdateB es)
        , ('d', keySetEditLevel EditDisplay es)
        , ('s', keySetEditLevel (EditScreen SelectedNone) es)
        , ('1', keyDigit allDs 1 es)
        , ('2', keyDigit allDs 2 es)
        , ('3', keyDigit allDs 3 es)
        , ('4', keyDigit allDs 4 es)
        , ('5', keyDigit allDs 5 es)
        , ('6', keyDigit allDs 6 es)
        , ('7', keyDigit allDs 7 es)
        , ('8', keyDigit allDs 8 es)
        , ('9', keyDigit allDs 9 es)
        ]
    >>= (\(k, mv) -> case mv of
          Nothing -> []
          Just v  -> return (k, v)
        )







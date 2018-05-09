{-# LANGUAGE TemplateHaskell #-}

module Menu
    ( Menu
    , create
    , handle
    , render
    ) where

import qualified Data.Map as M

import Base
import Button (Button(Button))
import qualified Button as B
import Image
import Level (Level)
import Levels (levels)
import Scores (Scores, getScore)

data Menu = Menu
    { _image :: Picture
    }

makeLenses ''Menu

create :: IO Menu
create = Menu <$> imgToPic 0.75 "assets/menu.png"

render :: Scores -> Menu -> Picture
render ss m = m ^. image
           <> foldMap B.render ((^. _1) <$> levelButtons ss)

handle :: Event -> Scores -> Maybe (Maybe (String, Level))
handle (EventKey (SpecialKey KeyEsc) Down _ _) _ = Just Nothing
handle e ss = go $ levelButtons ss
  where
    go [] = Nothing
    go ((b, n, l) : bs)
        | B.handle e b = Just (Just (n, l))
        | otherwise = go bs

levelButtons :: Scores -> [(Button, String, Level)]
levelButtons ss = zipWith mkLevelButton [-offset .. offset] (M.toList levels)
  where
    mkLevelButton i (n, l) = case getScore n ss of
        Nothing -> ( Button n (-200, 100 * i - 25) (200, 100 * i + 25), n, l )
        Just s -> ( Button (n ++ " - " ++ show s) (-200, 100 * i - 25) (200, 100 * i + 25), n, l )
    offset = fromIntegral (length levels - 1) / 2

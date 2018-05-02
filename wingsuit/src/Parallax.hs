{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Parallax
    ( load
    , render
    ) where

import Data.Maybe (fromJust)
import Graphics.Gloss.Juicy (fromDynamicImage)

import Base
import qualified Player as P

load :: IO [Picture]
load = sequence $ map (\path -> readImage ("assets/parallax/" <> path) >>= \case
                Left err -> error err
                Right img -> return $ scale 1.3 1.3 $ fromJust $ fromDynamicImage img
      )(reverse ["1 Layer1.png", "2 Layer2.png", "3 Layer3.png", "4 Layer4.png", "5 Mountains.png", "6 Sun.png", "7 Clouds.png", "8 Stars.png", "9 Background.png"])

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

render :: Point -> [Picture] -> Picture
render (x, y) pictures = Pictures $ reverse $ mapInd (\pic ind -> translate (x / 30 / (fromIntegral ind + 2)) (y / (-20) / (fromIntegral ind + 1)) pic) (reverse pictures)
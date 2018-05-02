{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Parallax
    ( load
    , render
    ) where

import Data.Maybe (fromJust)
import Graphics.Gloss.Juicy (fromDynamicImage)

import Base

load :: IO [Picture]
load = sequence $ map (\path -> readImage ("assets/parallax/" <> path) >>= \case
                Left err -> error err
                Right img -> return $ scale 1.5 1.5 $ fromJust $ fromDynamicImage img
      )(reverse ["1 Layer1.png", "2 Layer2.png", "3 Layer3.png", "4 Layer4.png", "5 Mountains.png", "6 Sun.png", "7 Clouds.png", "8 Stars.png", "9 Background.png"])

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [1..]

render :: Point -> Float -> [Picture] -> Picture
render (x, y) s pictures = Pictures $ reverse $ mapInd (\pic ind -> scale s s $ translate (x / (sqrt (fromIntegral ind) * 70)) (y / (sqrt (fromIntegral ind) * 20)) pic) (reverse pictures)
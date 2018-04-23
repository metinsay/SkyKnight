{-# LANGUAGE LambdaCase #-}
module Level.Image
    ( loadLevel
    ) where

import Codec.Picture
    ( convertRGBA8, readImage, pixelAt, PixelRGBA8(PixelRGBA8), imageWidth, imageHeight )
import Data.Maybe (fromJust)
import Graphics.Gloss.Juicy (fromDynamicImage)

import Base
import Level (Level(Level), _getIsTerrain, _getTerrain, _isFinish, _start)

isTerrain :: FilePath -> IO (Point -> Bool)
isTerrain path = readImage path >>= \case
    Left err -> error err
    Right img -> pure $ \(x, y) -> let
        conv = convertRGBA8 img
        pixel = pixelAt
            conv
            (imageWidth conv `div` 2 + round x)
            (imageHeight conv `div` 2 - round y)
        in pixel /= PixelRGBA8 0 0 0 0

terrain :: FilePath -> IO Picture
terrain path = readImage path >>= \case
    Left err -> error err
    Right img -> return $ fromJust $ fromDynamicImage img

loadLevel :: FilePath -> Point -> (Point -> Bool) -> Level
loadLevel path start isFinish = Level
    { _start = start
    , _isFinish = isFinish
    , _getIsTerrain = isTerrain $ path ++ "/collision.bmp"
    , _getTerrain = terrain $ path ++ "/art.bmp"
    }

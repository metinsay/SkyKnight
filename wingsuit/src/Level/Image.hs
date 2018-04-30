{-# LANGUAGE LambdaCase #-}

module Level.Image
    ( loadLevel
    ) where

import Codec.Picture
    ( convertRGBA8, readImage, pixelAt, PixelRGBA8(PixelRGBA8), imageWidth, imageHeight )
import Data.Maybe (fromJust)
import Graphics.Gloss.Juicy (fromDynamicImage)

import Base
import Block (Block)
import Level (Level(Level), _finish, _getIsTerrain, _getTerrain, _start, _startTime)

isTerrain :: FilePath -> IO (Point -> Bool)
isTerrain path = readImage path >>= \case
    Left err -> error err
    Right img -> pure $ \(x, y) -> let
        conv = convertRGBA8 img
        pixel = pixelAt
            conv
            (imageWidth conv `div` 2 + round (x / scaleFactor))
            (imageHeight conv `div` 2 - round (y / scaleFactor))
        in pixel /= PixelRGBA8 0 0 0 0

terrain :: FilePath -> IO Picture
terrain path = readImage path >>= \case
    Left err -> error err
    Right img -> return $ scale scaleFactor scaleFactor $ fromJust $ fromDynamicImage img

loadLevel :: FilePath -> Point -> Block -> Float -> Level
loadLevel path start finish startTime = Level
    { _start = start
    , _finish = finish
    , _getIsTerrain = isTerrain $ path ++ "/collision.bmp"
    , _getTerrain = terrain $ path ++ "/art.bmp"
    , _startTime = startTime
    }

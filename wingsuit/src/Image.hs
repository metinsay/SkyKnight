{-# LANGUAGE LambdaCase #-}

module Image (imgToFunc, imgToPic) where

import Codec.Picture (PixelRGBA8, convertRGBA8, imageHeight, imageWidth, pixelAt, readImage)
import Data.Maybe (fromJust)
import Graphics.Gloss.Juicy (fromDynamicImage)

import Base

imgToFunc :: FilePath -> IO (Point -> PixelRGBA8)
imgToFunc path = readImage path <&> \case
    Left err -> error err
    Right img -> \(x, y) -> let
        conv = convertRGBA8 img
        in pixelAt conv
                   (imageWidth conv `div` 2 + round (x / scaleFactor))
                   (imageHeight conv `div` 2 - round (y / scaleFactor))

imgToPic :: FilePath -> IO Picture
imgToPic path = readImage path <&> \case
    Left err -> error err
    Right img -> scale scaleFactor scaleFactor . fromJust $ fromDynamicImage img

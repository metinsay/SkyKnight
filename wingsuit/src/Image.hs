{-# LANGUAGE LambdaCase #-}

module Image (imgToFunc, imgToPic) where

import Data.Maybe (fromJust)

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

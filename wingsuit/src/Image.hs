{-# LANGUAGE LambdaCase #-}

module Image (imgToFunc, imgToPic) where

import Data.Maybe (fromJust)

import Base

imgToFunc :: Float -> FilePath -> IO (Point -> PixelRGBA8)
imgToFunc sf path = readImage path <&> \case
    Left err -> error err
    Right img -> \(x, y) -> let
        conv = convertRGBA8 img
        in pixelAt conv
                   (imageWidth conv `div` 2 + round (x / sf))
                   (imageHeight conv `div` 2 - round (y / sf))

imgToPic :: Float -> FilePath -> IO Picture
imgToPic sf path = readImage path <&> \case
    Left err -> error err
    Right img -> scale sf sf . fromJust $ fromDynamicImage img

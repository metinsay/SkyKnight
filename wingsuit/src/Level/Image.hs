{-# LANGUAGE LambdaCase #-}
module Level.Image
    ( loadLevel
    ) where

import Base
import Level (Level (Level))
import Codec.Picture (convertRGBA8, readImage, pixelAt, PixelRGBA8 (PixelRGBA8), imageWidth, imageHeight)
import Graphics.Gloss.Juicy (fromDynamicImage)
import Data.Maybe (fromJust)

isTerrain :: FilePath -> IO (Point -> Bool)
isTerrain path = readImage path >>= \case
                    Left err -> error err
                    Right img -> 
                        let conv = convertRGBA8 img
                        in 
                            return (\(x, y) -> pixelAt
                            conv
                            (imageWidth conv `div` 2 + round x)
                            (imageHeight conv `div` 2 - round y) /= PixelRGBA8 0 0 0 0)

terrain :: FilePath -> IO Picture
terrain path = readImage path >>= \case
                    Left err -> error err
                    Right img -> return $ fromJust $ fromDynamicImage img

--        Level folder-> start -> isFinish        -> returns level
loadLevel :: FilePath -> Point -> (Point -> Bool) -> Level
loadLevel path start isFinish = Level start isFinish (isTerrain $ path ++ "/collision.bmp") (terrain $ path ++ "/art.bmp")

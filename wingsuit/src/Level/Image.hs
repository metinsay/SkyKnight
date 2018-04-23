{-# LANGUAGE LambdaCase #-}
module Level.Image
    ( loadLevel
    ) where

import Base
import Level (Level (Level))
import Codec.Picture (convertRGB8, readBitmap, pixelAt, PixelRGB8 (PixelRGB8))
import Graphics.Gloss.Data.Bitmap (loadBMP)

isTerrain :: FilePath -> IO (Point -> Bool)
isTerrain path = readBitmap path >>= \case
                    Left err -> error err
                    Right img -> return (\(x, y) -> pixelAt (convertRGB8 img) (round x) (round y) /= PixelRGB8 0 0 0)

--        Level folder-> start -> isFinish        -> returns level
loadLevel :: FilePath -> Point -> (Point -> Bool) -> Level
loadLevel path start isFinish = Level start isFinish (isTerrain $ path ++ "/art.bmp") (loadBMP $ path ++ "/collision.bmp")

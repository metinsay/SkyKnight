{-# LANGUAGE LambdaCase #-}
module Image
    ( loadLevel
    ) where

import Base
import Level (Level (Level))
import Codec.Picture (readImage, pixelMap)

loadLevel :: FilePath -> IO Level
loadLevel path =
    readImage path >>= \case
        Left err -> error err
        Right img ->
            return $ Level (5, 5) ((100, 100), (150, 110)) [((5, 5), (5, 5))]

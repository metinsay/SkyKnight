module Level.Image (loadLevel) where

import Base
import Block (Block)
import Image
import Level (Level(Level), _finish, _getIsTerrain, _getTerrain, _start, _startTime)

isSolid :: PixelRGBA8 -> Bool
isSolid (PixelRGBA8 _ _ _ a) = a >= 128

loadLevel :: FilePath -> Point -> Block -> Float -> Level
loadLevel path start finish startTime = Level
    { _start = start
    , _finish = finish
    , _getIsTerrain = fmap isSolid <$> imgToFunc scaleFactor (path ++ "/collision.bmp")
    , _getTerrain = imgToPic scaleFactor $ path ++ "/art.bmp"
    , _startTime = startTime
    }

scaleFactor :: Float
scaleFactor = 20

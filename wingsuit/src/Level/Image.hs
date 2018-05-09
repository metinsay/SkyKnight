module Level.Image (loadLevel) where

import Codec.Picture (PixelRGBA8(PixelRGBA8))

import Base
import Image
import Block (Block)
import Level (Level(Level), _finish, _getIsTerrain, _getTerrain, _start, _startTime)

isSolid :: PixelRGBA8 -> Bool
isSolid (PixelRGBA8 _ _ _ a) = a >= 128

loadLevel :: FilePath -> Point -> Block -> Float -> Level
loadLevel path start finish startTime = Level
    { _start = start
    , _finish = finish
    , _getIsTerrain = fmap isSolid <$> imgToFunc (path ++ "/collision.bmp")
    , _getTerrain = imgToPic $ path ++ "/art.bmp"
    , _startTime = startTime
    }

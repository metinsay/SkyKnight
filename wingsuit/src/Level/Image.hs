module Level.Image (loadLevel) where

import Acorn
import Base
import Block (Block)
import Image
import Level (Level(Level), _finish, _getIsTerrain, _getTerrain, _start, _startTime, _acorns)

isSolid :: PixelRGBA8 -> Bool
isSolid (PixelRGBA8 _ _ _ a) = a >= 128

loadLevel :: FilePath -> Point -> Block -> Float -> [Point] -> Level
loadLevel path start finish startTime points = Level
    { _start = start
    , _finish = finish
    , _getIsTerrain = fmap isSolid <$> imgToFunc scaleFactor (path ++ "/collision.bmp")
    , _getTerrain = imgToPic scaleFactor $ path ++ "/art.bmp"
    , _startTime = startTime
    , _acorns = sequence $ map create points
    }

scaleFactor :: Float
scaleFactor = 20

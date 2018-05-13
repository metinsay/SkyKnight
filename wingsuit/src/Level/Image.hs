module Level.Image (loadLevel) where

import Acorn
import Base
import Block (Block)
import Image
import Level
    ( Level(Level), _finish, _getIsTerrain, _getTerrain, _getScaleXY, _start, _startTime, _acorns )

isSolid :: PixelRGBA8 -> Bool
isSolid (PixelRGBA8 _ _ _ a) = a >= 1

camScale :: Float
camScale = 1

getScaleXY :: (Point -> PixelRGBA8) -> (Point -> (Float, Float, Float))
getScaleXY readPix = \point -> let PixelRGBA8 x y z _ = readPix point in
    ( fromIntegral x * camScale / 128
    , fromIntegral y * camScale / 128
    , fromIntegral z * camScale / 128
    )

loadLevel :: FilePath -> Point -> Block -> Float -> [Point] -> Level
loadLevel path start finish startTime points = Level
    { _start = start
    , _finish = finish
    , _getIsTerrain = fmap isSolid <$> imgToFunc scaleFactor (path ++ "/collision.bmp")
    , _getTerrain = imgToPic scaleFactor $ path ++ "/art.bmp"
    , _getScaleXY = fmap getScaleXY <$> imgToFunc scaleFactor $ path ++ "/camera.bmp"
    , _startTime = startTime
    , _acorns = sequence $ map create points
    }

scaleFactor :: Float
scaleFactor = 20

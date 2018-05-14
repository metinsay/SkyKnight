{-# LANGUAGE TemplateHaskell #-}

module ImageButton
    ( ImageButton
    , handle
    , render
    , position
    , getRGB
    , image
    , create
    ) where

import Base
import Image

data ImageButton = ImageButton
    { _position :: Point
    , _getRGB :: Point -> PixelRGBA8
    , _image :: Picture
    }

makeLenses ''ImageButton

create :: FilePath -> Point -> IO ImageButton
create path pos = do rgbFn <- imgToFunc 1 path
                     ImageButton pos rgbFn <$> imgToPic 1 path


isSolid :: PixelRGBA8 -> Bool
isSolid (PixelRGBA8 _ _ _ a) = a >= 1

isCollision :: ImageButton -> Point -> Bool
isCollision b (x, y) = let
    (bx, by) = b ^. position
    mappedPoint = (x - bx, y - by)
    in x - bx > -97 && x - bx < 97 && y - by > -53 && y - by < 53
       && (isSolid <$> b ^. getRGB) mappedPoint

render :: ImageButton -> Picture
render b = translate x y (b ^. image)
  where
    (x, y) = b  ^. position

handle :: Event -> ImageButton -> Bool
handle (EventKey (MouseButton LeftButton) Down _ (x, y)) b
    = isCollision b (x, y)
handle _ _ = False

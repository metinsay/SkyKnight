{-# LANGUAGE TemplateHaskell #-}

module Acorn
    ( Acorn
    , collected
    , create
    , isCollision
    , position
    , render
    ) where

import Base
import Image

data Acorn = Acorn
    { _position :: Point
    , _collected :: Bool
    , _getRGB :: Point -> PixelRGBA8
    , _image :: Picture
    }

makeLenses ''Acorn

create :: Point -> IO Acorn
create pos = do rgbFn <- imgToFunc 1 ("assets/acorn.png")
                Acorn pos False rgbFn <$> imgToPic 1 "assets/acorn.png"

render :: Acorn -> Picture
render a = if a ^. collected then Blank else renderAcorn
  where
    (ax, ay) = a ^. position
    renderAcorn = translate ax ay (a ^. image)

isCollision :: Acorn -> Point -> Bool
isCollision a (x, y) = let
    (ax, ay) = a ^. position
    mappedPoint = (x - ax, y - ay)
    in x - ax > -190 && x - ax < 190 && y - ay > -190 && y - ay < 190
        && (isSolid <$> a ^. getRGB) mappedPoint

isSolid :: PixelRGBA8 -> Bool
isSolid (PixelRGBA8 _ _ _ a) = a >= 1

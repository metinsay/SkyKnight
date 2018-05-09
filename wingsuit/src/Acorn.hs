{-# LANGUAGE TemplateHaskell #-}

module Acorn
    ( Acorn
    , create
    , render
    , isCollision

    , collected
    ) where

import Base
import Image

isSolid :: PixelRGBA8 -> Bool
isSolid (PixelRGBA8 _ _ _ a) = a >= 1

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
        in x - ax > -200 && x - ax < 200 && y - ay > -200 && y - ay < 200 && (isSolid <$> a ^. getRGB) mappedPoint

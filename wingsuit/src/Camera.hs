{-# LANGUAGE TemplateHaskell #-}

module Camera
    ( Camera(Camera)
    , _xPos
    , _yPos
    , _zPos
    , combine
    , render
    ) where

import Base

data Camera = Camera
    { _xPos :: Float
    , _yPos :: Float
    , _zPos :: Float
    }

makeLenses ''Camera

render :: Camera -> Float -> Picture -> Picture
render c z p = join scale (recip $ c ^. zPos - z) $ translate (- c ^. xPos) (- c ^. yPos) p

combine :: Float -> Camera -> Camera -> Camera
combine w c1 c2 = Camera
    { _xPos = (1 - w) * c1 ^. xPos + w * c2 ^. xPos
    , _yPos = (1 - w) * c1 ^. yPos + w * c2 ^. yPos
    , _zPos = (1 - w) * c1 ^. zPos + w * c2 ^. zPos
    }

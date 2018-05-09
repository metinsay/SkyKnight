{-# LANGUAGE TemplateHaskell #-}

module Acorn
    ( Acorn
    , create
    , render
    , isCollision
    ) where

import Base
import Image

data Acorn = Acorn
    { _position :: Point
    , _hidden :: Bool
    , _image :: Picture
    } deriving Show

makeLenses ''Acorn

create :: Point -> IO Acorn
create pos = Acorn pos False <$> imgToPic 1 "assets/acorn.png"


render :: Acorn -> Picture
render a = if a ^. hidden then Blank else renderAcorn
               where
                  (ax, ay) = a ^. position
                  renderAcorn = translate ax ay (a ^. image)

isCollision :: Acorn -> Point -> Bool
isCollision a p = False
-- TODO implement this


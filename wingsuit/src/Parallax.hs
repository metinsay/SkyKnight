{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Parallax
    ( create
    , render
    ) where

import Base
import Camera (Camera)
import qualified Camera as C
import Image

create :: IO [Picture]
create = traverse (\(i, n) -> imgToPic i ("assets/parallax/" <> n)) $ zip [20, 40 ..]
    [ "1 Layer1.png", "2 Layer2.png", "3 Layer3.png", "4 Layer4.png", "5 Mountains.png"
    , "6 Sun.png", "7 Clouds.png", "8 Stars.png", "9 Background.png"
    ]

render :: Camera -> [Picture] -> Picture
render c ps = fold . reverse $ (\(i, p) -> C.render c i p) <$> zip [-10, -20 ..] ps

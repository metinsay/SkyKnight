{-# LANGUAGE TemplateHaskell #-}
module PauseMenu
    ( PauseAction(Play, Quit, Reset)
    , PauseMenu
    , create
    , handle
    , render
    ) where

import Base
import Image

data PauseAction = Play | Quit | Reset

data PauseMenu = PauseMenu
    { _image :: Picture
    }

makeLenses ''PauseMenu

create :: IO PauseMenu
create = PauseMenu <$> imgToPic 1 "assets/pause.png"

render :: PauseMenu -> Picture
render p = p ^. image


handle :: Event -> Maybe PauseAction
handle _ = Nothing

module PauseMenu
    ( PauseAction(Play, Quit, Reset)
    , handle
    , render
    ) where

import Base

data PauseAction = Play | Quit | Reset

render :: Picture
render = mempty

handle :: Event -> Maybe PauseAction
handle _ = Nothing

{-# LANGUAGE TemplateHaskell #-}

module PauseMenu
    ( PauseAction(Play, Quit, Reset)
    , PauseMenu
    , create
    , handle
    , render
    ) where

import Base
import Button (Button(Button))
import qualified Button as B
import Image

data PauseAction = Play | Quit | Reset

data PauseMenu = PauseMenu
    { _image :: Picture
    }

makeLenses ''PauseMenu

create :: IO PauseMenu
create = PauseMenu <$> imgToPic 1 "assets/pause.png"

render :: PauseMenu -> Picture
render p = translate 0 100 (p ^. image)
        <> foldMap B.render ((^. _1) <$> pauseButtons)

handle :: Event -> Maybe PauseAction
handle e = case filter (B.handle e . fst) pauseButtons of
    [] -> case e of
        EventKey (MouseButton LeftButton) Down _ _ -> Just Play
        _ -> Nothing
    ((_, a) : _) -> Just a

pauseButtons :: [(Button, PauseAction)]
pauseButtons =
    [ (Button "Restart (r)" (-200, -125) (200, -75), Reset)
    , (Button "Quit (q)" (-200, -225) (200, -175), Quit)
    ]

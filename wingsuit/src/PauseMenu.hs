{-# LANGUAGE TemplateHaskell #-}

module PauseMenu
    ( PauseAction(Play, Quit, Reset)
    , PauseMenu
    , create
    , handle
    , render
    ) where

import Base
import ImageButton (ImageButton)
import qualified ImageButton as IB
import Image

data PauseAction = Play | Quit | Reset

data PauseMenu = PauseMenu
    { _image :: Picture ,
      _buttons :: [(ImageButton, PauseAction)]
    }

makeLenses ''PauseMenu

create :: IO PauseMenu
create = do
           pB <- pauseButtons
           pic <- imgToPic 1 "assets/pause.png"
           pure $ PauseMenu pic pB

render :: PauseMenu -> Picture
render p = translate 0 100 (p ^. image)
        <> foldMap IB.render ((^. _1) <$> (p ^. buttons))

handle :: Event -> PauseMenu -> Maybe PauseAction
handle e pm = case filter (IB.handle e . fst) (pm ^. buttons) of
    [] -> case e of
        EventKey (MouseButton LeftButton) Down _ _ -> Just Play
        _ -> Nothing
    ((_, a) : _) -> Just a

pauseButtons :: IO [(ImageButton, PauseAction)]
pauseButtons = do
                 restart <- IB.create "assets/buttons/restart_button.png" (0, -125)
                 quit <- IB.create "assets/buttons/quit_button.png" (0, -225)
                 pure $ [ (restart , Reset), (quit, Quit) ]

module Menu
    ( handle
    , render
    ) where

import Base
import Button (Button(Button))
import qualified Button as B
import Level (Level)
import qualified Level as L

render :: Picture
render = B.render level1Button
      <> B.render level2Button
      <> B.render level3Button

handle :: Event -> Maybe Level
handle e
    | B.handle e level1Button = Just L.level1
    | B.handle e level2Button = Just L.level2
    | B.handle e level3Button = Just L.level3
handle _ = Nothing

level1Button :: Button
level1Button = Button "level1" (-200, 175) (200, 225)

level2Button :: Button
level2Button = Button "level2" (-200, -25) (200, 25)

level3Button :: Button
level3Button = Button "level3" (-200, -225) (200, -175)

module FinishHud (render) where

import Base
import World (World, acornCount, time)

render :: Float -> World -> Picture
render score w = wonText <> timeText <> acornText <> scoreText
  where
    wonText   = fuzz
              . scale 0.4 0.4
              . color (makeColor 1 1 0 1)
              . translate (-500) 400
              . text
              $ "You won!"

    timeText  = fuzz
              . scale 0.4 0.4
              . color (makeColor 1 1 0 1)
              . translate (-500) 100
              . text
              $ "Time left: " ++ showFFloat (Just 2) (w ^. time) "s"

    acornText = fuzz
              . scale 0.4 0.4
              . color (makeColor 1 1 0 1)
              . translate (-500) (-200)
              . text
              $ "Acorns: " ++ showFFloat (Just 0) (acornCount w) " * 3"

    scoreText = fuzz
              . scale 0.4 0.4
              . color (makeColor 1 1 0 1)
              . translate (-500) (-500)
              . text
              $ "Score: " ++ showFFloat (Just 2) score ""

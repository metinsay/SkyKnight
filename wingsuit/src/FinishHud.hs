module FinishHud (render) where

import Base
import World (World, acornCount, time)

render :: Float -> World -> Picture
render score w = wonText <> timeText <> acornText <> scoreText
  where
    wonText   = scale 0.4 0.4
              . color (makeColor 1 1 0 1)
              . translate (-500) 400
              . text
              $ "You won!"

    timeText  = scale 0.4 0.4
              . color (makeColor 1 1 0 1)
              . translate (-500) 100
              . text
              $ "Time: " ++ showFFloat (Just 2) (w ^. time) "s"

    acornText = scale 0.4 0.4
              . color (makeColor 1 1 0 1)
              . translate (-500) (-200)
              . text
              $ "Acorns: 5 * " ++ show (acornCount w)

    scoreText = scale 0.4 0.4
              . color (makeColor 1 1 0 1)
              . translate (-500) (-500)
              . text
              $ "Score: " ++ showFFloat (Just 2) score ""

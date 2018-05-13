module FinishHud (render) where

import Base
import World (World, time)

render :: Float -> World -> Picture
render score w = wonText <> scoreText <> timeText
   where
     wonText   = scale 0.4 0.4
               . color (makeColor 1 1 0 1)
               . translate (500) 300
               . text
               $ "You won!"

     scoreText = scale 0.4 0.4
               . color (makeColor 1 1 0 1)
               . translate (500) 0
               . text
               $ "Score: " ++ showFFloat (Just 2) score ""

     timeText  = scale 0.4 0.4
               . color (makeColor 1 1 0 1)
               . translate (500) (-300)
               . text
               $ "Time: " ++ showFFloat (Just 2) (w ^. time) "s"

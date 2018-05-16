{-# LANGUAGE MultiWayIf #-}

module FinishHud (render) where

import Base
import World (World, acornCount, cutoffs, time)

render :: Float -> World -> Picture
render score w = wonText <> timeText <> acornText <> scoreText <> extraText1 <> extraText2
  where
    wonText    = fuzz
               . scale 0.4 0.4
               . color (makeColor 1 1 0 1)
               . translate (-500) 400
               . text
               $ "You won!"

    timeText   = fuzz
               . scale 0.4 0.4
               . color (makeColor 1 1 0 1)
               . translate (-500) 200
               . text
               $ "Time left: " ++ showFFloat (Just 2) (w ^. time) "s"

    acornText  = fuzz
               . scale 0.4 0.4
               . color (makeColor 1 1 0 1)
               . translate (-500) 0
               . text
               $ "Acorns: " ++ showFFloat (Just 0) (acornCount w) " * 3"

    scoreText  = fuzz
               . scale 0.4 0.4
               . color (makeColor 1 1 0 1)
               . translate (-500) (-200)
               . text
               $ "Score: " ++ showFFloat (Just 2) score ""

    extraText1
        = fuzz
        . scale 0.4 0.4
        . color (makeColor 1 1 0 1)
        . translate (-500) (-400)
        . text
        $ if | score > w ^. cutoffs . _3 -> "Gold!"
             | score > w ^. cutoffs . _2 -> "Silver!"
             | score > w ^. cutoffs . _1 -> "Bronze!"
             | otherwise -> "Need " ++ show (w ^. cutoffs . _1) ++ " for Bronze!"

    extraText2
        = fuzz
        . scale 0.4 0.4
        . color (makeColor 1 1 0 1)
        . translate (-500) (-600)
        . text
        $ if | score > w ^. cutoffs . _3 -> ""
             | score > w ^. cutoffs . _2 -> "Need " ++ show (w ^. cutoffs . _3) ++ " for Gold"
             | score > w ^. cutoffs . _1 -> "Need " ++ show (w ^. cutoffs . _2) ++ " for Silver"
             | otherwise -> ""

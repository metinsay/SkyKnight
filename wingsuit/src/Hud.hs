{-# LANGUAGE TypeApplications #-}

module Hud (render) where

import Base
import qualified Player as P
import World (World)
import qualified World as W

render :: World -> Picture
render w = renderHeightText
        <> renderSpeedText
        <> renderAccelerationText
        <> renderTime
        <> renderScore
  where
    renderHeightText = showText 300 . show @Int . floor . snd $ w ^. W.player . P.position
    renderSpeedText = showText 200 . show @Int . floor . mag $ w ^. W.player . P.velocity
    renderAccelerationText = showText 100 . show @Int . floor . mag $ P.acceleration (w ^. W.player)
    renderTime = showText 0 $ showFFloat (Just 3) (w ^. W.time) ""
    renderScore = showText (-100) $ showFFloat (Just 3) (w ^. W.score) ""
    showText y t = color (makeColor 0 0.8 0 1) . translate 400 y . scale 0.5 0.5 $ text t

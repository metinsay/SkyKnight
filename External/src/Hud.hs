{-# LANGUAGE TypeApplications #-}

module Hud (render) where

import Base
import Player (Player)
import qualified Player as P

render :: Player -> Picture
render p = renderHeightText <> renderSpeedText <> renderAccelerationText
  where
    renderHeightText = showText 300 . show @Int . floor . snd $ p ^. P.position
    renderSpeedText = showText 200 . show @Int . floor . mag $ p ^. P.velocity
    renderAccelerationText = showText 100 . show @Int . floor . mag $ P.acceleration p
    showText y t = color (makeColor 0 0.8 0 1) . translate 400 y . scale 0.5 0.5 $ text t

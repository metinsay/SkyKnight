{-# LANGUAGE TemplateHaskell, TypeApplications #-}

module Hud (Hud, create, render) where

import Base
import qualified Block as B
import Image
import qualified Player as P
import World (World)
import qualified World as W

data Hud = Hud
    { _progressBar :: Picture
    , _progressLoc :: Picture
    }

makeLenses ''Hud

create :: IO Hud
create = Hud <$> imgToPic 1 "assets/progress_bar.png" <*> imgToPic 1 "assets/progress_loc.png"

render :: World -> Hud -> Picture
render w h = renderHeightText
          <> renderSpeedText
          <> renderAccelerationText
          <> renderTime
          <> renderScore
          <> renderProgress
  where
    renderHeightText = showText 300 . show @Int . floor . snd $ w ^. W.player . P.position
    renderSpeedText = showText 200 . show @Int . floor . mag $ w ^. W.player . P.velocity
    renderAccelerationText = showText 100 . show @Int . floor . mag $ P.acceleration (w ^. W.player)
    renderTime = showText 0 $ showFFloat (Just 3) (w ^. W.time) ""
    renderScore = showText (-100) $ showFFloat (Just 3) (w ^. W.score) ""
    renderProgress = translate 0 300
                   $ h ^. progressBar
                  <> translate (1000 * (0.5 - curDist / initDist)) 0 (h ^. progressLoc)
    initDist = mag $ B.center (w ^. W.finish) - w ^. W.start
    curDist = mag $ B.center (w ^. W.finish) - w ^. W.player . P.position
    showText y t = color (makeColor 0 0.8 0 1) . translate 400 y . scale 0.5 0.5 $ text t

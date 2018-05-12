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
    , _speedometer :: Picture
    , _arrow :: Picture
    }

makeLenses ''Hud

create :: IO Hud
create = Hud <$> imgToPic 1 "assets/progress_bar.png"
             <*> imgToPic 1 "assets/progress_loc.png"
             <*> imgToPic 0.6 "assets/speedometer.png"
             <*> imgToPic 0.6 "assets/arrow.png"

render :: World -> Hud -> Picture
render w h = renderProgress <> renderSpeed
  where
    renderProgress = translate 0 300
                   $ h ^. progressBar
                  <> translate (1000 * (0.5 - curDist / initDist)) 0 (h ^. progressLoc)
    initDist = mag $ B.center (w ^. W.finish) - w ^. W.start
    curDist = mag $ B.center (w ^. W.finish) - w ^. W.player . P.position
    renderSpeed = renderSpeedometer <> renderArrow
    renderSpeedometer = translate (-500) (-300) $ h ^. speedometer
    renderArrow
        = translate (-500) (-330)
        . rotate (mag (w ^. W.player . P.velocity) / 10 - 110)
        . translate 0 30
        $ h ^. arrow

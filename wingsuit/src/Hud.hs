{-# LANGUAGE TemplateHaskell, TypeApplications #-}

module Hud (Hud, create, render) where

import Base
import qualified Acorn as A
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
    , _acorn :: Picture
    }

makeLenses ''Hud

create :: IO Hud
create = Hud <$> imgToPic 1 "assets/progress_bar.png"
             <*> imgToPic 1 "assets/progress_loc.png"
             <*> imgToPic 0.6 "assets/speedometer.png"
             <*> imgToPic 0.6 "assets/arrow.png"
             <*> imgToPic 0.2 "assets/acorn.png"

render :: World -> Hud -> Picture
render w h = renderProgress <> renderAcorns <> renderSpeed
  where
    renderProgress
         = translate 0 300 (h ^. progressBar)
        <> translate (getProgress (w ^. W.player . P.position)) 300 (h ^. progressLoc)

    renderAcorns = foldMap
        (\a -> translate (getProgress (a ^. A.position)) 300
             $ bool id (scale 0.5 0.5) (a ^. A.collected) (h ^. acorn)
        )
        (w ^. W.acorns)

    getProgress p = 500 - 1000 * getDist p / totalDist
    getDist p = mag $ B.center (w ^. W.finish) - p
    totalDist = mag $ B.center (w ^. W.finish) - w ^. W.start

    renderSpeed = renderSpeedometer <> renderArrow
    renderSpeedometer = translate (-500) (-300) $ h ^. speedometer
    renderArrow
        = translate (-500) (-330)
        . rotate (mag (w ^. W.player . P.velocity) / 10 - 110)
        . translate 0 30
        $ h ^. arrow

{-# LANGUAGE TemplateHaskell #-}

module World
    ( World
    , getScaleXY
    , acorns
    , create
    , finish
    , player
    , render
    , reset
    , start
    , step
    , time
    ) where

import Acorn (Acorn)
import qualified Acorn as A
import Base
import Block (Block)
import Camera (Camera)
import qualified Camera as C
import qualified Block as B
import Level (Level)
import qualified Level as L
import Player (Player)
import qualified Player as P

data World = World
    { _player :: Player
    , _start :: Point
    , _finish :: Block
    , _isTerrain :: Point -> Bool
    , _terrain :: Picture
    , _getScaleXY :: Point -> (Float, Float, Float)
    , _startTime :: Float
    , _time :: Float
    , _acorns :: [Acorn]
    }

makeLenses ''World

render :: Camera -> World -> Picture
render c w = C.render c 0
           $ w ^. terrain
          <> P.render (w ^. player)
          <> Pictures (A.render <$> w ^. acorns)

step :: Float -> Point -> World -> (Maybe (Maybe Float), World)
step t c
    = checkFinish
    . checkCollision
    . checkTime
    . (,) Nothing
    . updateAcorns
    . (time -~ t)
    . (player %~ P.step t c)
  where
    checkFinish (e, w) = bool (e, w) (Just . Just $ w ^. time + 5 * accornCount w, w)
        . or $ flip B.inBlock (w ^. finish) <$> P.points (w ^. player)
    checkCollision (e, w) = bool (e, w) (Just Nothing, w)
        . or $ w ^. isTerrain <$> P.points (w ^. player)
    checkTime (e, w) = bool (e, w) (Just Nothing, w) (w ^. time < 0)
    updateAcorns w = w & acorns %~ map (updateAcorn $ P.points (w ^. player))

accornCount :: World -> Float
accornCount = fromIntegral . length . filter (^. A.collected) . (^. acorns)

updateAcorn :: [Point] -> Acorn -> Acorn
updateAcorn ps a = bool a (a & A.collected .~ True) $ any (A.isCollision a) ps

create :: Level -> IO World
create l = do
    isTer <- l ^. L.getIsTerrain
    ter <- l ^. L.getTerrain
    p <- P.create $ l ^. L.start
    ac <- l ^. L.acorns
    getScale <- l ^. L.getScaleXY
    pure $ World
        { _player = p
        , _isTerrain = isTer
        , _terrain = ter
        , _getScaleXY = getScale
        , _start = l ^. L.start
        , _finish = l ^. L.finish
        , _startTime = l ^. L.startTime
        , _time = l ^. L.startTime
        , _acorns = ac
        }

reset :: World -> World
reset w = w & player %~ P.reset (w ^. start) & time .~ w ^. startTime
        & acorns %~ map (\a -> a & A.collected .~ False)

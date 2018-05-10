{-# LANGUAGE TemplateHaskell #-}

module World
    ( World
    , create
    , finish
    , handle
    , player
    , playerGroundDist
    , playerTerrainDist
    , render
    , score
    , start
    , step
    , time
    ) where

import Base
import Block (Block)
import qualified Block as B
import Level (Level)
import qualified Level as L
import Player (Player)
import qualified Player as P
import qualified Acorn as A

data World = World
    { _player :: Player
    , _start :: Point
    , _finish :: Block
    , _isTerrain :: Point -> Bool
    , _terrain :: Picture
    , _startTime :: Float
    , _time :: Float
    , _score :: Float
    , _acorns :: [A.Acorn]
    }

makeLenses ''World

render :: World -> Picture
render w = w ^. terrain
        <> P.render (w ^. player)
        <> Pictures (A.render <$> w ^. acorns)

handle :: Event -> World -> World
handle (EventKey (Char 'r') Down _ _) w = reset w
handle _ w = w

step :: Float -> Point -> World -> (Maybe Float, World)
step t c
    = checkFinish
    . checkTime
    . checkCollision
    . updateScore
    . updateAcorns
    . (time -~ t)
    . (player %~ P.step t c)
  where
    checkFinish w = bool (Nothing, w) (Just $ w ^. time + w ^. score, w)
        . or $ flip B.inBlock (w ^. finish) <$> P.points (w ^. player)
    checkTime w = bool w (reset w) (w ^. time < 0)
    checkCollision w = bool w (reset w) . or $ w ^. isTerrain <$> P.points (w ^. player)
    updateAcorns w = w & acorns %~ map (updateAcorn $ P.points (w ^. player))
    updateScore w = w & score +~ max 0 (t * (1 - playerGroundDist w / 500))

updateAcorn :: [Point] -> A.Acorn -> A.Acorn
updateAcorn ps a = bool a (a & A.collected .~ True) $ any (A.isCollision a) ps

create :: Level -> IO World
create l = do
    isTer <- l ^. L.getIsTerrain
    ter <- l ^. L.getTerrain
    p <- P.create $ l ^. L.start
    ac <- l ^. L.acorns
    pure $ World
        { _player = p
        , _isTerrain = isTer
        , _terrain = ter
        , _start = l ^. L.start
        , _finish = l ^. L.finish
        , _startTime = l ^. L.startTime
        , _time = l ^. L.startTime
        , _score = 0
        , _acorns = ac
        }

reset :: World -> World
reset w = w & player %~ P.reset (w ^. start) & time .~ w ^. startTime & score .~ 0 & acorns %~ map (\a -> a & A.collected .~ False)

playerGroundDist :: World -> Float
playerGroundDist w = groundDist w (w ^. player ^. P.position) (0, -1)

groundDist :: World -> Point -> Point -> Float
groundDist w (x, y) (dx, dy) = if (w ^. isTerrain) (x, y)
    then 0
    else 1 + groundDist w (x + dx, y + dy) (dx, dy)

playerTerrainDist :: World -> Float
playerTerrainDist w = minimum
      $ (\angle -> groundDist w (w ^. player ^. P.position) angle)
    <$> [(0, -1), (-1, -1), (1, -1), (-1, -2), (1, -2)]

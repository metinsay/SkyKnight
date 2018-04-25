{-# LANGUAGE TemplateHaskell #-}

module World
    ( World
    , create
    , handle
    , player
    , render
    , step
    , time
    ) where

import Base
import Level (Level)
import qualified Level as L
import Player (Player)
import qualified Player as P

data World = World
    { _player :: Player
    , _start :: Point
    , _isFinish :: Point -> Bool
    , _isTerrain :: Point -> Bool
    , _terrain :: Picture
    , _startTime :: Float
    , _time :: Float
    , _score :: Float
    }

makeLenses ''World

render :: World -> Picture
render w = w ^. terrain
        <> P.render (w ^. player)

handle :: Event -> World -> World
handle (EventKey (Char 'r') Down _ _) w = w & player %~ P.reset (w ^. start)
handle e w = w & player %~ P.handle e

step :: Float -> World -> (Maybe Float, World)
step t = (checkFinish &&& id) . checkTime . checkCollision . (player %~ P.step t) . (time -~ t)
  where
    checkFinish w = bool Nothing (Just $ w ^. time + w ^. score)
        $ (w ^. isFinish $ w ^. player . P.position)
    checkTime w = bool w (reset w) (w ^. time < 0)
    checkCollision w = bool w (reset w) . or $ w ^. isTerrain <$> P.points (w ^. player)

create :: Level -> IO World
create l = do
    isTer <- l ^. L.getIsTerrain
    ter <- l ^. L.getTerrain
    p <- P.create $ l ^. L.start
    pure $ World
        { _player = p
        , _isTerrain = isTer
        , _terrain = ter
        , _start = l ^. L.start
        , _isFinish = l ^. L.isFinish
        , _startTime = l ^. L.startTime
        , _time = l ^. L.startTime
        , _score = 0
        }

reset :: World -> World
reset w = w & player %~ P.reset (w ^. start) & time .~ w ^. startTime

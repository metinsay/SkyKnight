{-# LANGUAGE TemplateHaskell #-}

module World
    ( World
    , create
    , handle
    , player
    , playerGroundDist
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
    , _time :: Float
    }

makeLenses ''World

render :: World -> Picture
render w = w ^. terrain
        <> P.render (w ^. player)

handle :: Event -> World -> World
handle (EventKey (Char 'r') Down _ _) w = w & player %~ P.reset (w ^. start)
handle e w = w & player %~ P.handle e

step :: Float -> World -> (Bool, World)
step t = (checkFinish &&& id) . checkCollision . (player %~ P.step t) . (time +~ t)
  where
    checkFinish w = w ^. isFinish $ w ^. player . P.position
    checkCollision w = bool w (w & player %~ P.reset (w ^. start) & time .~ 0)
        . or $ w ^. isTerrain <$> P.points (w ^. player)

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
        , _time = 0
        }

playerGroundDist :: World -> Float
playerGroundDist w = groundDist w $ w ^. player ^. P.position

groundDist :: World -> Point -> Float
groundDist w (x, y) = if (w ^. isTerrain) (x, y) then 0 else 1 + groundDist w (x, y-1)
{-# LANGUAGE TemplateHaskell #-}

module World
    ( World
    , blocks
    , create
    , handle
    , player
    , render
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

data World = World
    { _player :: Player
    , _blocks :: [Block]
    , _start :: Point
    , _finish :: Block
    , _time :: Float
    } deriving Show

makeLenses ''World

render :: World -> Picture
render w = P.render (w ^. player)
        <> foldMap B.render (w ^. blocks)
        <> color (makeColor 0 1 0 1) (B.render $ w ^. finish)

handle :: Event -> World -> World
handle (EventKey (Char 'r') Down _ _) w = w & player %~ P.reset (w ^. start)
handle e w = w & player %~ P.handle e

step :: Float -> World -> (Bool, World)
step t = (checkFinish &&& id) . checkCollision . (player %~ P.step t) . (time +~ t)
  where
    checkFinish w = B.inBlock (w ^. player . P.position) (w ^. finish)
    checkCollision w = bool w (w & player %~ P.reset (w ^. start) & time .~ 0)
        . or $ B.inBlock <$> P.points (w ^. player) <*> w ^. blocks

create :: Level -> World
create l = World
    { _player = P.create $ l ^. L.start
    , _blocks = l ^. L.blocks
    , _start = l ^. L.start
    , _finish = l ^. L.finish
    , _time = 0
    }

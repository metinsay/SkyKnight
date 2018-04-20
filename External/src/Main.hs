{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Base
import Block (Block)
import qualified Block as B
import qualified Hud as H
import qualified Level as L
import Player (Player)
import qualified Player as P

data State = State
    { _player :: Player
    , _blocks :: [Block]
    , _start :: Point
    , _finish :: Block
    , _time :: Float
    , _done :: Bool
    } deriving Show

makeLenses ''State

main :: IO ()
main = play FullScreen (makeColor 1 1 1 1) 60 initial render handle step

initial :: State
initial = State
    { _player = P.create $ L.level ^. L.start
    , _blocks = L.level ^. L.blocks
    , _start = L.level ^. L.start
    , _finish = L.level ^. L.finish
    , _time = 0
    , _done = False
    }

render :: State -> Picture
render s = join scale scaling (uncurry translate (- s ^. player . P.position) world)
        <> H.render (s ^. time) (s ^. player)
  where
    world = (P.render $ s ^. player)
         <> (foldMap B.render $ s ^. blocks)
         <> (color (makeColor 0 1 0 1) (B.render $ s ^. finish))
    scaling = 400 / sqrt (100000 + dist ** 2)
    dist = foldl' min 10000 $
        mag . subtract (s ^. player . P.position) <$> (B.points =<< s ^. blocks)

handle :: Event -> State -> State
handle e s = s & player %~ P.handle e

step :: Float -> State -> State
step t = checkFinish . checkCollision . (player %~ P.step t) . updateTime
  where
    updateTime s = bool (s & time +~ t) s (s ^. done)
    checkFinish s = bool s (s & done .~ True) (B.inBlock (s ^. player . P.position) (s ^. finish))
    checkCollision s = bool s (s & player %~ P.reset (s ^. start) & time .~ 0 & done .~ False)
        . or $ B.inBlock <$> P.points (s ^. player) <*> s ^. blocks

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
render s = uncurry translate (- s ^. player . P.position) (P.render $ s ^. player)
        <> uncurry translate (- s ^. player . P.position) (foldMap B.render $ s ^. blocks)
        <> uncurry translate (- s ^. player . P.position)
              (color (makeColor 0 1 0 1) (B.render $ s ^. finish))
        <> H.render (s ^. time) (s ^. player)

handle :: Event -> State -> State
handle e s = s & player %~ P.handle e

step :: Float -> State -> State
step t = checkFinish . checkCollision . (player %~ P.step t) . updateTime
  where
    updateTime s = bool (s & time +~ t) s (s ^. done)
    checkFinish s = bool s (s & done .~ True) (B.inBlock (s ^. player . P.position) (s ^. finish))
    checkCollision s = bool s (s & player %~ P.reset (s ^. start) & time .~ 0 & done .~ False)
        . or $ B.inBlock <$> P.points (s ^. player) <*> s ^. blocks

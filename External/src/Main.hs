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
    } deriving Show

makeLenses ''State

main :: IO ()
main = play FullScreen (makeColor 1 1 1 1) 60 initial render handle step

initial :: State
initial = State (P.create $ L.getStart L.level) (L.getBlocks L.level) (L.getStart L.level)

render :: State -> Picture
render s = uncurry translate (- P.getPosition (s ^. player)) (P.render $ s ^. player)
        <> uncurry translate (- P.getPosition (s ^. player)) (foldMap B.render $ s ^. blocks)
        <> H.render (s ^. player)

handle :: Event -> State -> State
handle e s = s & player %~ P.handle e

step :: Float -> State -> State
step t = checkCollision . (player %~ P.step t)
  where
    checkCollision s = s & player %~ \p -> bool p (P.reset (s ^. start) p) . or
        $ inBlock <$> P.getPoints p <*> s ^. blocks

inBlock :: Point -> (Point, Point) -> Bool
inBlock (x, y) ((x1, y1), (x2, y2)) = x > x1 && x < x2 && y > y1 && y < y2

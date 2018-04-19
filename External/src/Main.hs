{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Base
import Block (Block)
import qualified Block as B
import qualified Hud as H
import Player (Player)
import qualified Player as P

initialPlayer :: Point
initialPlayer = (50, 11500)

initialBlocks :: [Block]
initialBlocks = [ ((-1000, -1000), (0, 12000)) ]
    <|> (\(x, y) -> ((x, y - 10000), (x + 50, y))) <$> zip [0, 50 ..] heights
  where
    heights = [ (6000 + cos (x / 50) * 4000) * 0.9985 ** x | x <- [0 .. 2000] ]
    -- heights = [ 6000 * (sin (x / 50) / (2 ** ((x / 50 - pi / 2) / pi))) | x <- [2 * 50 .. 1500] ]

data State = State
    { _player :: Player
    , _blocks :: [Block]
    } deriving Show

makeLenses ''State

main :: IO ()
main = play FullScreen (makeColor 1 1 1 1) 60 initial render handle step

initial :: State
initial = State (P.create initialPlayer) initialBlocks

render :: State -> Picture
render s = uncurry translate (- P.getPosition (s ^. player)) (P.render $ s ^. player)
        <> uncurry translate (- P.getPosition (s ^. player)) (foldMap B.render $ s ^. blocks)
        <> H.render (s ^. player)

handle :: Event -> State -> State
handle e s = s & player %~ P.handle e

step :: Float -> State -> State
step t = checkCollision . (player %~ P.step t)
  where
    checkCollision s = s & player %~ \p -> bool p (P.reset initialPlayer p) . or
        $ inBlock <$> P.getPoints p <*> s ^. blocks

inBlock :: Point -> (Point, Point) -> Bool
inBlock (x, y) ((x1, y1), (x2, y2)) = x > x1 && x < x2 && y > y1 && y < y2

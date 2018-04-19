{-# LANGUAGE TemplateHaskell, TypeApplications #-}

module Main (main) where

import Control.Applicative ((<|>))
import Control.Lens
import Data.Bool (bool)
import Data.Monoid ((<>))
import Graphics.Gloss.Interface.Pure.Game

import Base
import Block (Block)
import qualified Block as B
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
main = play display background framerate initial render handle step

display :: Display
display = FullScreen

background :: Color
background = makeColor 1 1 1 1

framerate :: Int
framerate = 60

initial :: State
initial = State (P.create initialPlayer) initialBlocks

render :: State -> Picture
render s = uncurry translate (- P.getPosition p) (P.render p <> foldMap B.render (s ^. blocks))
    <> renderHeightText <> renderSpeedText <> renderAccelerationText
  where
    p = s ^. player
    renderHeightText = showText 300 . show @Int . floor $ P.getPosition p ^. _2
    renderSpeedText = showText 200 . show @Int . floor . mag $ P.getVelocity p
    renderAccelerationText = showText 100 . show @Int . floor . mag $ P.getAcceleration p
    showText y t = color (makeColor 0 0.8 0 1) . translate 400 y . scale 0.5 0.5 $ text t

handle :: Event -> State -> State
handle e s = s & player %~ P.handle e

step :: Float -> State -> State
step t = checkCollision . (player %~ P.step t)
  where
    checkCollision s = s & player %~ \p -> bool p (P.reset initialPlayer p) . or
        $ inBlock <$> P.getPoints p <*> s ^. blocks

inBlock :: Point -> (Point, Point) -> Bool
inBlock (x, y) ((x1, y1), (x2, y2)) = x > x1 && x < x2 && y > y1 && y < y2

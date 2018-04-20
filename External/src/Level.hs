{-# LANGUAGE TemplateHaskell #-}

module Level
    ( Level
    , blocks
    , level
    , start
    ) where

import Base
import Block

data Level = Level
    { _start :: Point
    , _blocks :: [Block]
    } deriving Show

makeLenses ''Level

level :: Level
level = Level initialPlayer initialBlocks

initialPlayer :: Point
initialPlayer = (50, 11500)

initialBlocks :: [Block]
initialBlocks = [ ((-1000, -1000), (0, 12000)) ]
    <|> (\(x, y) -> ((x, y - 10000), (x + 50, y))) <$> zip [0, 50 ..] heights
  where
    heights = [ (6000 + cos (x / 50) * 4000) * 0.9985 ** x | x <- [0 .. 2000] ]
    -- heights = [ 6000 * (sin (x / 50) / (2 ** ((x / 50 - pi / 2) / pi))) | x <- [2 * 50 .. 1500] ]

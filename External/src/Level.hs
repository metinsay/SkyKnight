{-# LANGUAGE TemplateHaskell #-}

module Level
    ( Level
    , blocks
    , finish
    , level
    , start
    ) where

import Base
import Block

data Level = Level
    { _start :: Point
    , _finish :: Block
    , _blocks :: [Block]
    } deriving Show

makeLenses ''Level

level :: Level
level = Level (50, 11500) ((99950, 200), (100050, 300)) $ [ ((-4000, -1000), (0, 14000)) ]
    <|> (\(x, y) -> ((x, y - 10000), (x + 50, y))) <$> zip [0, 50 ..] heights
  where
    heights = [ (6000 + cos (x / 50) * 4000) * 0.9985 ** x | x <- [0 .. 2000] ]
    -- heights = [ 6000 * (sin (x / 50) / (2 ** ((x / 50 - pi / 2) / pi))) | x <- [2 * 50 .. 1500] ]

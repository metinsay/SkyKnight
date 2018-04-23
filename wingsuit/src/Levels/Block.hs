{-# LANGUAGE OverloadedLists #-}

module Levels.Block (levels) where

import Data.Map (Map)

import Base
import Block (Block)
import qualified Block as B
import Level (Level(Level), _getIsTerrain, _getTerrain, _isFinish, _start)

levels :: Map String Level
levels =
    [ ("level1", level1)
    , ("level2", level2)
    , ("level3", level3)
    ]

level1 :: Level
level1 = mkLevel 11500 ((99950, 200), (100050, 300))
    $ [ (6000 + cos (x / 50) * 4000) * 0.9985 ** x | x <- [0 .. 2000] ]

level2 :: Level
level2 = mkLevel 8500 ((69950, 50), (70050, 150))
    $ [ 6000 * (sin (x / 50) / (2 ** ((x / 50 - pi / 2) / pi))) | x <- [2 * 50 .. 1500] ]

level3 :: Level
level3 = mkLevel 10500 ((36550, 3050), (36650, 3150))
     $ [ x ** 4 / 10000 | x <- [-100 .. 69] ]
    ++ [ 4802 - x ** 4 / 10000 | x <- [-70 .. 69] ]
    ++ [ x ** 4 / 10000 | x <- [-70 .. 0] ]
    ++ [ x * 10 | x <- [0 .. 300] ]
    ++ replicate 50 3000

mkLevel :: Float -> Block -> [Float] -> Level
mkLevel s f heights = Level
    { _start = (50, s)
    , _isFinish = flip B.inBlock f
    , _getIsTerrain = pure $ \p -> or $ B.inBlock p <$> blocks
    , _getTerrain = pure $ foldMap B.render blocks
                 <> color (makeColor 0 1 0 1) (B.render f)
    }
  where
    blocks = [ ((-4000, -1000), (0, 14000)) ]
         <|> (\(x, y) -> ((x, y - 10000), (x + 50, y))) <$> zip [0, 50 ..] heights

{-# LANGUAGE OverloadedLists #-}

module Levels.Image (levels) where

import Data.Map (Map)
import Level.Image (loadLevel)

import Level (Level)

levels :: Map String Level
levels = [ ("level1", level1)
         , ("level2", level2)
         , ("level3", level3)
         ]

level1 :: Level
level1 = loadLevel "levels/level1" (-19750, 4750) ((19900, -5000), (20000, 5000)) 100

level2 :: Level
level2 = loadLevel "levels/level2" (-19500, 4750) ((19900, -5000), (20000, 5000)) 100

level3 :: Level
level3 = loadLevel "levels/level3" (-1400, 400) ((0, 0), (0, 0)) 100

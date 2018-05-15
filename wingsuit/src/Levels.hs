{-# LANGUAGE OverloadedLists #-}

module Levels (levels) where

import Data.Map (Map)

import ImageLevel (loadLevel)
import Level (Level)

levels :: Map String Level
levels = [ ("Level 4", level4)
         , ("Level 3", level3)
         , ("Level 2", level2)
         , ("Level 1", level1)
         ]

level1 :: Level
level1 = loadLevel
    "levels/level1"
    (-19750, 4750)
    ((19900, -5000), (20000, 5000))
    50
    [(-19500, 4000), (-15000, -2900), (-7500, -850), (0, -1250), (6000, -4400), (13500,-1900)]

level2 :: Level
level2 = loadLevel
    "levels/level2"
    (-19750, 4750)
    ((19900, -5000), (20000, 5000))
    60
    [(-19500, 4000), (-15000, 500), (-7500, 0), (0, -1800), (9000, -1000), (17000,-1800)]

level3 :: Level
level3 = loadLevel
    "levels/level3"
    (-19500, 4750)
    ((19900, -5000), (20000, 5000))
    50
    [(-19250, 4100), (-15200, 500), (-8600, -2650), (900, -4500), (7000, -1500), (13500, -3000)]

level4 :: Level
level4 = loadLevel
    "levels/level4"
    (-19500, 4750)
    ((19900, -5000), (20000, 5000))
    60
    [(-19500, 4000), (-15000, 1200), (-7500, -3000), (0, -3080), (9000, 0), (17000,-2000)]

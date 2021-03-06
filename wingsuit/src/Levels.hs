{-# LANGUAGE OverloadedLists #-}

module Levels (levels) where

import Data.Map (Map)

import ImageLevel (loadLevel)
import Level (Level)

levels :: Map String Level
levels = [ ("Level 3", level3)
         , ("Level 2", level2)
         , ("Level 1", level1)
         ]

level1 :: Level
level1 = loadLevel
    "levels/level1"
    (-19750, 4750)
    ((19900, -5000), (20000, 5000))
    50
    (22, 27, 31)
    [(-19500, 4000), (-15000, 500), (-7500, 0), (0, -1800), (9000, -1000), (17000,-1800)]

level2 :: Level
level2 = loadLevel
    "levels/level2"
    (-19450, 4850)
    ((19900, -5000), (20000, 5000))
    50
    (20, 25, 29)
    [(-19250, 4100), (-15200, 500), (-8600, -2650), (900, -4500), (7000, -1500), (13500, -3000)]

level3 :: Level
level3 = loadLevel
    "levels/level3"
    (-19500, 4750)
    ((19900, -5000), (20000, 5000))
    60
    (18, 24, 28)
    [(-19500, 4000), (-15000, 1200), (-7500, -3000), (0, -3080), (9000, 0), (17000,-2000)]

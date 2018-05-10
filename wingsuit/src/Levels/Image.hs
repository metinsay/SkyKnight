{-# LANGUAGE OverloadedLists #-}

module Levels.Image (levels) where

import Data.Map (Map)
import Level.Image (loadLevel)

import Level (Level)

levels :: Map String Level
levels = [ ("level1", level1)
         , ("level2", level2)
         , ("level3", level3)
         , ("level1_easy", level1_easy)
         , ("level2_easy", level2_easy)
         , ("level3_easy", level3_easy)
         , ("level4", level4)
         , ("chris_01", chris_01)
         ]

level1 :: Level
level1 = loadLevel "levels/level1" (-19750, 4750) ((19900, -5000), (20000, 5000)) 100 [(-19500, 4000), (-15000, -2900), (-7500, -1050), (0, -1400), (6000, -4400), (13500,-1900)]

level2 :: Level
level2 = loadLevel "levels/level2" (-19500, 4750) ((19900, -5000), (20000, 5000)) 100 [(-19250, 4100), (-15200, 500), (-8600, -2650), (900, -4500), (7000, -1500), (13500, -3000)]

level3 :: Level
level3 = loadLevel "levels/level3" (-19500, 4750) ((19900, -5000), (20000, 5000)) 100 [(-19500, 4000), (-15000, 1200), (-7500, -3000), (0, -3080), (9000, 0), (17000,-2000)]

level1_easy :: Level
level1_easy = loadLevel "levels/level1_easy" (-19750, 4750) ((19900, -5000), (20000, 5000)) 100 [(-19500, 4000), (-15000, -2900), (-7500, -900), (0, -1350), (6000, -4400), (13500,-1900)]

level2_easy :: Level
level2_easy = loadLevel "levels/level2_easy" (-19500, 4750) ((19900, -5000), (20000, 5000)) 100 [(-19250, 4100), (-15200, 500), (-8600, -2650), (900, -4500), (7000, -1500), (13500, -3000)]

level3_easy :: Level
level3_easy = loadLevel "levels/level3_easy" (-19500, 4750) ((19900, -5000), (20000, 5000)) 100[(-19500, 4000), (-15000, 1200), (-7500, -3000), (0, -3080), (9000, 0), (17000,-2000)]

level4 :: Level
level4 = loadLevel "levels/level4" (-19750, 4750) ((19900, -5000), (20000, 5000)) 100 [(-19500, 4000), (-15000, 500), (-7500, 0), (0, -1800), (9000, -1000), (17000,-1800)]

chris_01 :: Level
chris_01 = loadLevel "levels/chris_01" (-19750, 4750) ((19900, -5000), (20000, 5000)) 100 []

{-# LANGUAGE OverloadedLists #-}

module Levels.Image (levels) where

import Data.Map (Map)
import Level.Image (loadLevel)

import Level (Level)
import Base (scaleFactor)

levels :: Map String Level
levels = [ ("level4", level1)
         , ("level5", level2)
         , ("level6", level3)
         ]

level1 :: Level
level1 = loadLevel "levels/level1" (-3950 * scaleFactor, 950 * scaleFactor) (const False)

level2 :: Level
level2 = loadLevel "levels/level2" (-1400, 400) (const False)

level3 :: Level
level3 = loadLevel "levels/level3" (-1400, 400) (const False)

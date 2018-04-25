{-# LANGUAGE OverloadedLists #-}

module Levels.Image (levels) where

import Data.Map (Map)
import Level.Image (loadLevel)

import Level (Level)
import Base (scaleFactor)

levels :: Map String Level
levels = [ ("level1", level1)
         , ("level2", level2)
         , ("level3", level3)
         ]

level1 :: Level
level1 = loadLevel "levels/level1" (-1350 * scaleFactor, 480 * scaleFactor) (const False) 100

level2 :: Level
level2 = loadLevel "levels/level2" (-1400, 400) (const False) 100

level3 :: Level
level3 = loadLevel "levels/level3" (-1400, 400) (const False) 100

module Levels (levels) where

import Data.Map (Map)

import Level (Level)
import qualified Levels.Block as BL

levels :: Map String Level
levels = BL.levels

module Levels (levels) where

import Data.Map (Map)

import Base
import Level (Level)
import qualified Levels.Block as BL
import qualified Levels.Image as IL

levels :: Map String Level
levels = IL.levels <> BL.levels

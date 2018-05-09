{-# LANGUAGE OverloadedLists, TemplateHaskell #-}

module Level
    ( Level(Level)
    , _finish
    , _acorns
    , _getIsTerrain
    , _getTerrain
    , _start
    , _startTime
    , finish
    , acorns
    , getIsTerrain
    , getTerrain
    , start
    , startTime
    ) where

import Acorn
import Base
import Block (Block)

data Level = Level
    { _start :: Point
    , _finish :: Block
    , _getIsTerrain :: IO (Point -> Bool)
    , _getTerrain :: IO Picture
    , _startTime :: Float
    , _acorns :: IO [Acorn]
    }

makeLenses ''Level

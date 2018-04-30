{-# LANGUAGE OverloadedLists, TemplateHaskell #-}

module Level
    ( Level(Level)
    , _finish
    , _getIsTerrain
    , _getTerrain
    , _start
    , _startTime
    , finish
    , getIsTerrain
    , getTerrain
    , start
    , startTime
    ) where

import Base
import Block (Block)

data Level = Level
    { _start :: Point
    , _finish :: Block
    , _getIsTerrain :: IO (Point -> Bool)
    , _getTerrain :: IO Picture
    , _startTime :: Float
    }

makeLenses ''Level

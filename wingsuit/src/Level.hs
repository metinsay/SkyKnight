{-# LANGUAGE OverloadedLists, TemplateHaskell #-}

module Level
    ( Level(Level)
    , _acorns
    , _cutoffs
    , _finish
    , _getIsTerrain
    , _getTerrain
    , _getScaleXY
    , _start
    , _startTime
    , acorns
    , cutoffs
    , finish
    , getIsTerrain
    , getTerrain
    , getScaleXY
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
    , _getScaleXY :: IO (Point -> (Float, Float, Float))
    , _startTime :: Float
    , _cutoffs :: (Float, Float, Float)
    , _acorns :: IO [Acorn]
    }

makeLenses ''Level

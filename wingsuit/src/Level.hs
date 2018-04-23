{-# LANGUAGE OverloadedLists, TemplateHaskell #-}

module Level
    ( Level(Level)
    , _getIsTerrain
    , _getTerrain
    , _isFinish
    , _start
    , getIsTerrain
    , getTerrain
    , isFinish
    , start
    ) where

import Base

data Level = Level
    { _start :: Point
    , _isFinish :: Point -> Bool
    , _getIsTerrain :: IO (Point -> Bool)
    , _getTerrain :: IO Picture
    }

makeLenses ''Level

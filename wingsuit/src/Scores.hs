{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scores where

import qualified Data.Map as M

import Base

newtype Scores = Scores (Map String Float)
    deriving (FromJSON, ToJSON)

instance Monoid Scores where
    mempty = Scores mempty
    Scores s1 `mappend` Scores s2 = Scores $ M.unionWith max s1 s2

getScore :: String -> Scores -> Maybe Float
getScore n (Scores ss) = M.lookup n ss

updateScore :: String -> Float -> Scores -> Scores
updateScore n s (Scores ss) = Scores $ M.insertWith max n s ss

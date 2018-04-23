module Scores where

import Data.Map (Map)
import qualified Data.Map as M

newtype Scores = Scores (Map String Float)

instance Monoid Scores where
    mempty = Scores mempty
    Scores s1 `mappend` Scores s2 = Scores $ M.unionWith min s1 s2

getScore :: String -> Scores -> Maybe Float
getScore n (Scores ss) = M.lookup n ss

updateScore :: String -> Float -> Scores -> Scores
updateScore n s (Scores ss) = Scores $ M.insertWith min n s ss

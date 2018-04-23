module Menu
    ( handle
    , render
    ) where

import qualified Data.Map as M

import Base
import Button (Button(Button))
import qualified Button as B
import Level (Level)
import qualified Level as L
import Scores (Scores, getScore)

render :: Scores -> Picture
render ss = foldMap B.render $ fst <$> levelButtons ss

handle :: Scores -> Event -> Maybe (String, Level)
handle ss e = go $ levelButtons ss
  where
    go [] = Nothing
    go ((b@(Button n _ _), l) : bs)
        | B.handle e b = Just (n, l)
        | otherwise = go bs

levelButtons :: Scores -> [(Button, Level)]
levelButtons ss = zipWith mkLevelButton [-offset .. offset] (M.toList L.levels)
  where
    mkLevelButton i (n, l) = case getScore n ss of
        Nothing -> ( Button n (-200, 100 * i - 25) (200, 100 * i + 25), l )
        Just s -> ( Button (n ++ " - " ++ show s) (-200, 100 * i - 25) (200, 100 * i + 25), l )
    offset = fromIntegral (length L.levels - 1) / 2

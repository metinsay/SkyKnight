module Menu
    ( handle
    , render
    ) where

import qualified Data.Map as M

import Base
import Button (Button(Button))
import qualified Button as B
import Level (Level)
import Levels (levels)
import Scores (Scores, getScore)

render :: Scores -> Picture
render ss = foldMap B.render $ (^. _1) <$> levelButtons ss

handle :: Scores -> Event -> Maybe (String, Level)
handle ss e = go $ levelButtons ss
  where
    go [] = Nothing
    go ((b, n, l) : bs)
        | B.handle e b = Just (n, l)
        | otherwise = go bs

levelButtons :: Scores -> [(Button, String, Level)]
levelButtons ss = zipWith mkLevelButton [-offset .. offset] (M.toList levels)
  where
    mkLevelButton i (n, l) = case getScore n ss of
        Nothing -> ( Button n (-200, 100 * i - 25) (200, 100 * i + 25), n, l )
        Just s -> ( Button (n ++ " - " ++ show s) (-200, 100 * i - 25) (200, 100 * i + 25), n, l )
    offset = fromIntegral (length levels - 1) / 2

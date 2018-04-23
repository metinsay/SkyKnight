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

render :: Picture
render = foldMap B.render $ fst <$> levelButtons

handle :: Event -> Maybe Level
handle e = go levelButtons
  where
    go [] = Nothing
    go ((b, l) : bs)
        | B.handle e b = Just l
        | otherwise = go bs

levelButtons :: [(Button, Level)]
levelButtons = zipWith mkLevelButton [-offset .. offset] (M.toList L.levels)
  where
    mkLevelButton i (n, l) = ( Button n (-200, 100 * i - 25) (200, 100 * i + 25), l )
    offset = fromIntegral (length L.levels - 1) / 2

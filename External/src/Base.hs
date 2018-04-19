module Base
    ( module Control.Lens
    , module Data.Monoid
    , module Graphics.Gloss.Interface.Pure.Game
    , mag
    , unit
    , (.*)
    ) where

import Control.Lens (makeLenses, (&), (+~), (.~), (^.))
import Data.Monoid ((<>))
import Graphics.Gloss.Interface.Pure.Game
    ( Event(EventMotion), Picture, Point, color, line, makeColor, polygon )

unit :: Point -> Point
unit p = if mag p == 0 then 0 else 1 / mag p .* p

mag :: Point -> Float
mag (x, y) = sqrt $ x ** 2 + y ** 2

(.*) :: Float -> Point -> Point
m .* (x, y) = (m * x, m * y)
infixl 7 .*

module Base
    ( module Control.Applicative
    , module Control.Lens
    , module Data.Bool
    , module Data.Monoid
    , module Graphics.Gloss.Interface.Pure.Game
    , mag
    , unit
    , (.*)
    ) where

import Control.Applicative ((<|>))
import Control.Lens (makeLenses, (&), (%~), (+~), (.~), (^.))
import Data.Bool (bool)
import Data.Monoid ((<>))
import Graphics.Gloss.Interface.Pure.Game
    ( Display(FullScreen), Event(EventMotion), Picture, Point
    , color, line, makeColor, play, polygon, scale, text, translate
    )

unit :: Point -> Point
unit p = if mag p == 0 then 0 else 1 / mag p .* p

mag :: Point -> Float
mag (x, y) = sqrt $ x ** 2 + y ** 2

(.*) :: Float -> Point -> Point
m .* (x, y) = (m * x, m * y)
infixl 7 .*

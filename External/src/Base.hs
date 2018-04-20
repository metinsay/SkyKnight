module Base
    ( module Control.Applicative
    , module Control.Arrow
    , module Control.Monad
    , module Control.Lens
    , module Data.Bool
    , module Data.List
    , module Data.Monoid
    , module Graphics.Gloss.Interface.Pure.Game
    , mag
    , unit
    , (.*)
    ) where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Lens (makeLenses, (&), (%~), (+~), (.~), (^.))
import Control.Monad (join)
import Data.Bool (bool)
import Data.List (foldl')
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

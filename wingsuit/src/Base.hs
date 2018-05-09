module Base
    ( module Codec.Picture
    , module Control.Applicative
    , module Control.Arrow
    , module Control.Monad
    , module Control.Lens
    , module Data.Bool
    , module Data.List
    , module Data.Map
    , module Data.Monoid
    , module Graphics.Gloss.Interface.IO.Game
    , module Graphics.Gloss.Juicy
    , module Numeric
    , module System.Exit
    , mag
    , physicsFactor
    , unit
    , (.*)
    ) where

import Codec.Picture
    ( PixelRGBA8(PixelRGBA8), convertRGBA8, imageHeight, imageWidth, pixelAt, readImage )
import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Lens (_1, makeLenses, makePrisms, (&), (%~), (+~), (-~), (.~), (<&>), (^.))
import Control.Monad (join)
import Data.Bool (bool)
import Data.List (foldl')
import Data.Map (Map)
import Data.Monoid ((<>))
import Graphics.Gloss.Interface.IO.Game
    ( Display(FullScreen)
    , Event(EventKey, EventMotion)
    , Key(Char, MouseButton, SpecialKey)
    , KeyState(Down)
    , MouseButton(LeftButton)
    , Picture(Pictures)
    , Point
    , SpecialKey(KeyEsc)
    , color, line, makeColor, playIO, polygon, rotate, scale, text, translate
    )
import Graphics.Gloss.Juicy (fromDynamicImage)
import Numeric (showFFloat)
import System.Exit (exitSuccess)

unit :: Point -> Point
unit p = if mag p == 0 then 0 else 1 / mag p .* p

mag :: Point -> Float
mag (x, y) = sqrt $ x ** 2 + y ** 2

(.*) :: Float -> Point -> Point
m .* (x, y) = (m * x, m * y)
infixl 7 .*

physicsFactor :: Float
physicsFactor = 1

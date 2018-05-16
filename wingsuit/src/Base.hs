module Base
    ( module Codec.Picture
    , module Control.Applicative
    , module Control.Arrow
    , module Control.Monad
    , module Control.Lens
    , module Data.Bool
    , module Data.Foldable
    , module Data.List
    , module Data.Map
    , module Data.Monoid
    , module Graphics.Gloss.Interface.IO.Game
    , module Graphics.Gloss.Juicy
    , module Numeric
    , module System.Exit
    , module System.Random
    , fuzz
    , mag
    , unit
    , (.*)
    ) where

import Codec.Picture
    ( PixelRGBA8(PixelRGBA8), convertRGBA8, imageHeight, imageWidth, pixelAt, readImage )
import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Lens (_1, _2, _3, makeLenses, makePrisms, (&), (%~), (+~), (-~), (.~), (<&>), (^.))
import Control.Monad (join)
import Data.Bool (bool)
import Data.Foldable (fold)
import Data.List (foldl', intercalate)
import Data.Map (Map)
import Data.Monoid ((<>))
import Graphics.Gloss.Interface.IO.Game
    ( Color
    , Display(FullScreen, InWindow)
    , Event(EventKey, EventMotion)
    , Key(Char, MouseButton, SpecialKey)
    , KeyState(Down)
    , MouseButton(LeftButton)
    , Picture(Pictures, Blank)
    , Point
    , SpecialKey(KeyEsc)
    , color, line, makeColor, playIO, polygon, rotate, scale, text, translate
    )
import Graphics.Gloss.Juicy (fromDynamicImage)
import Numeric (showFFloat)
import System.Exit (exitSuccess)
import System.Random (randomIO)

fuzz :: Picture -> Picture
fuzz p = translate 0 0 p
      <> translate 0 1 p
      <> translate 1 0 p
      <> translate 1 1 p

unit :: Point -> Point
unit p = if mag p == 0 then 0 else 1 / mag p .* p

mag :: Point -> Float
mag (x, y) = sqrt $ x ** 2 + y ** 2

(.*) :: Float -> Point -> Point
m .* (x, y) = (m * x, m * y)
infixl 7 .*

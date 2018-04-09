{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Applicative ((<|>))
import Control.Lens
import Data.Bool (bool)
import Data.Foldable (fold)
import Data.Monoid ((<>))
import Graphics.Gloss.Interface.Pure.Game

gravity :: Float
gravity = 200

drag :: Float
drag = 0.01

getLift :: Float -> Float
getLift x = 2 * x ** 0.3

rotVel :: Float
rotVel = 3

initialPlayer :: Point
initialPlayer = (50, 10500)

initialBlocks :: [(Point, Point)]
initialBlocks = [ ((-1000, -1000), (0, 12000)) ]
    <|> (\(x, y) -> ((x, -1000), (x + 50, y))) <$> zip [0, 50 ..] heights
  where
    heights = (/ 10000) . (^ 4) <$> [-100 .. 69]
          <|> (4802 -) . (/ 10000) . (^ 4) <$> [-70 .. 69]
          <|> (/ 10000) . (^ 4) <$> [-70 .. 0]
          <|> (* 10) <$> [0 .. 300]
          <|> replicate 50 3000
          <|> replicate 50 10000

data State = State
    { _position :: Point
    , _velocity :: Point
    , _rotation :: Point
    , _blocks :: [(Point, Point)]
    } deriving Show
makeLenses ''State

main :: IO ()
main = play display background framerate initial render handle step

display :: Display
display = FullScreen

background :: Color
background = makeColor 1 1 1 1

framerate :: Int
framerate = 60

initial :: State
initial = State initialPlayer 0 (1, 0) initialBlocks

reset :: State -> State
reset s = s & position .~ initialPlayer & velocity .~ 0

render :: State -> Picture
render s = translate (- s ^. position . _1) (- s ^. position . _2)
    $ renderPlayer <> fold renderBlocks
  where
    renderPlayer = polygon $ playerPoints s
    renderBlocks = s ^. blocks <&> \((x1, y1), (x2, y2)) ->
        polygon [(x1, y1), (x1, y2), (x2, y2), (x2, y1)]

handle :: Event -> State -> State
handle (EventMotion (x, y)) s = s & rotation .~ unit (x, y)
handle _ s = s

step :: Float -> State -> State
step t s = s & updateVelocity & updatePosition & checkCollision
  where
    aim = unit $ s ^. rotation
    norm = (\(x, y) -> (-y, x)) aim
    dir = unit $ s ^. velocity
    offset = uncurry (+) (norm * dir)
    lift = signum offset * getLift (abs offset)
    updateVelocity s = s
        & velocity +~ (0, - gravity * t)
        & velocity *~ ((1 - drag) ** t) .* 1
        & velocity -~ lift * t * mag (s ^. velocity) .* norm
    updatePosition s = s & position +~ t .* s ^. velocity
    checkCollision s = bool s (reset s) . or $ inBlock <$> playerPoints s <*> s ^. blocks

playerPoints :: State -> [Point]
playerPoints s =
    [ (x + 20 * dx - 5 * dy, y + 20 * dy + 5 * dx)
    , (x + 20 * dx + 5 * dy, y + 20 * dy - 5 * dx)
    , (x - 20 * dx + 5 * dy, y - 20 * dy - 5 * dx)
    , (x - 20 * dx - 5 * dy, y - 20 * dy + 5 * dx)
    ]
  where
    (x, y) = s ^. position
    (dx, dy) = s ^. rotation

inBlock :: Point -> (Point, Point) -> Bool
inBlock (x, y) ((x1, y1), (x2, y2)) = x > x1 && x < x2 && y > y1 && y < y2

unit :: Point -> Point
unit p = if mag p == 0 then 0 else 1 / mag p .* p

mag :: Point -> Float
mag (x, y) = sqrt $ x ** 2 + y ** 2

(.*) :: Float -> Point -> Point
m .* (x, y) = (m * x, m * y)
infixl 7 .*

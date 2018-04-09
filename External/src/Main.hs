{-# LANGUAGE TemplateHaskell, TypeApplications #-}

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

getLift :: Float -> Float -> Float
getLift v x = 0.002 * v ** 2 * x ** 0.3

initialPlayer :: Point
initialPlayer = (50, 10500)

initialBlocks :: [(Point, Point)]
initialBlocks = [ ((-1000, -1000), (0, 12000)) ]
    <|> (\(x, y) -> ((x, -1000), (x + 50, y))) <$> zip [0, 50 ..] heights
  where
    heights = [ x ** 4 / 10000 | x <- [-100 .. 69] ]
           ++ [ 4802 - x ** 4 / 10000 | x <- [-70 .. 69] ]
           ++ [ x ** 4 / 10000 | x <- [-70 .. 0] ]
           ++ [ x * 10 | x <- [0 .. 300] ]
           ++ replicate 50 3000
           ++ replicate 50 10000

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
render s = uncurry translate (- s ^. position) (renderPlayer <> fold renderBlocks)
    <> renderHeight <> renderSpeed <> renderAcceleration
  where
    renderHeight = showText 300 . show @Int . floor $ s ^. position ^. _2
    renderSpeed = showText 200 . show @Int . floor . mag $ s ^. velocity
    renderAcceleration = showText 100 . show @Int . floor . mag $ getAcceleration s
    showText y t = color (makeColor 0 0.8 0 1) . translate 400 y . scale 0.5 0.5 $ text t
    renderPlayer = polygon $ playerPoints s
    renderBlocks = s ^. blocks <&> \((x1, y1), (x2, y2)) ->
        polygon [(x1, y1), (x1, y2), (x2, y2), (x2, y1)]

handle :: Event -> State -> State
handle (EventMotion (x, y)) s = s & rotation .~ unit (x, y)
handle _ s = s

step :: Float -> State -> State
step t = checkCollision . updatePosition . updateVelocity 
  where
    updateVelocity s = s & velocity +~ t .* getAcceleration s
    updatePosition s = s & position +~ t .* s ^. velocity
    checkCollision s = bool s (reset s) . or $ inBlock <$> playerPoints s <*> s ^. blocks

getAcceleration :: State -> Point
getAcceleration s = (0, - gravity) - drag .* s ^. velocity - lift .* norm
  where
    aim = unit $ s ^. rotation
    norm = (\(x, y) -> (-y, x)) aim
    offset = uncurry (+) (norm * dir)
    dir = unit $ s ^. velocity
    lift = signum offset * getLift (mag $ s ^. velocity) (abs offset)

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

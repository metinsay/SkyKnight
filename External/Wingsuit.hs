{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Graphics.Gloss.Interface.Pure.Game

gravity :: Float
gravity = 100

drag :: Float
drag = 0.01

lift :: Float
lift = 2

rotVel :: Float
rotVel = 3

data State = State
    { _position :: (Float, Float)
    , _velocity :: (Float, Float) 
    , _rotation :: Float
    , _keys :: (Bool, Bool)
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
initial = State (-400, 300) (0, 0) 1 (False, False)

render :: State -> Picture
render (State (x, y) _ r _) = polygon
    [ (x + 20 * cos r - 5 * sin r, y + 20 * sin r + 5 * cos r)
    , (x + 20 * cos r + 5 * sin r, y + 20 * sin r - 5 * cos r)
    , (x - 20 * cos r + 5 * sin r, y - 20 * sin r - 5 * cos r)
    , (x - 20 * cos r - 5 * sin r, y - 20 * sin r + 5 * cos r)
    ]

handle :: Event -> State -> State
handle (EventKey (Char 'a') Down _ _) s = s & keys . _1 .~ True
handle (EventKey (Char 'a') Up _ _) s = s & keys . _1 .~ False
handle (EventKey (Char 'd') Down _ _) s = s & keys . _2 .~ True
handle (EventKey (Char 'd') Up _ _) s = s & keys . _2 .~ False
handle _ s = s

step :: Float -> State -> State
step t s = s
    & velocity +~ (0, - gravity * t)
    & velocity *~ ((1 - drag) ** t) .* 1
    & velocity -~ lift * t * uncurry (+) (norm * s ^. velocity) .* norm
    & position +~ t .* s ^. velocity
    & rotation +~ rotVel * t * fromIntegral (fromEnum $ s ^. keys . _1)
    & rotation -~ rotVel * t * fromIntegral (fromEnum $ s ^. keys . _2)
  where
    norm = (- sin (s ^. rotation), cos (s ^. rotation))

(.*) :: Float -> (Float, Float) -> (Float, Float)
m .* (x, y) = (m * x, m * y)
infixl 7 .*

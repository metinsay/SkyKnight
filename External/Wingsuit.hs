{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Data.Bool (bool)
import Data.Foldable (fold)
import Data.Monoid ((<>))
import Graphics.Gloss.Interface.Pure.Game

gravity :: Float
gravity = 100

drag :: Float
drag = 0.01

lift :: Float
lift = 2

rotVel :: Float
rotVel = 3

initialBlocks =
    [ ( (-650, -400), (-350, -50) )
    , ( (-650, -400), (150, -200) )
    ]

data State = State
    { _position :: Point
    , _velocity :: Point
    , _rotation :: Float
    , _keys :: (Bool, Bool)
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
initial = State (-600, 350) (0, 0) 0 (False, False) initialBlocks

render :: State -> Picture
render s = renderPlayer <> fold renderBlocks
  where
    renderPlayer = polygon $ playerPoints s
    renderBlocks = s ^. blocks <&> \((x1, y1), (x2, y2)) ->
        polygon [(x1, y1), (x1, y2), (x2, y2), (x2, y1)]

handle :: Event -> State -> State
handle (EventKey (Char 'a') Down _ _) s = s & keys . _1 .~ True
handle (EventKey (Char 'a') Up _ _) s = s & keys . _1 .~ False
handle (EventKey (Char 'd') Down _ _) s = s & keys . _2 .~ True
handle (EventKey (Char 'd') Up _ _) s = s & keys . _2 .~ False
handle _ s = s

step :: Float -> State -> State
step t s = s & updateVelocity & updatePosition & updateRotation & checkCollision
  where
    norm = (- sin (s ^. rotation), cos (s ^. rotation))
    updateVelocity s = s
        & velocity +~ (0, - gravity * t)
        & velocity *~ ((1 - drag) ** t) .* 1
        & velocity -~ lift * t * uncurry (+) (norm * s ^. velocity) .* norm
    updatePosition s = s & position +~ t .* s ^. velocity
    updateRotation s = s
        & rotation +~ rotVel * t * fromIntegral (fromEnum $ s ^. keys . _1)
        & rotation -~ rotVel * t * fromIntegral (fromEnum $ s ^. keys . _2)
    checkCollision s = bool s initial . or $ inBlock <$> playerPoints s <*> s ^. blocks

playerPoints :: State -> [Point]
playerPoints s =
    [ (x + 20 * cos r - 5 * sin r, y + 20 * sin r + 5 * cos r)
    , (x + 20 * cos r + 5 * sin r, y + 20 * sin r - 5 * cos r)
    , (x - 20 * cos r + 5 * sin r, y - 20 * sin r - 5 * cos r)
    , (x - 20 * cos r - 5 * sin r, y - 20 * sin r + 5 * cos r)
    ]
  where
    (x, y) = s ^. position
    r = s ^. rotation

(.*) :: Float -> Point -> Point
m .* (x, y) = (m * x, m * y)
infixl 7 .*

inBlock :: Point -> (Point, Point) -> Bool
inBlock (x, y) ((x1, y1), (x2, y2)) = x > x1 && x < x2 && y > y1 && y < y2

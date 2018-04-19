{-# LANGUAGE TemplateHaskell #-}

module Player
    ( Player
    , create
    , getAcceleration
    , getPoints
    , getPosition
    , getVelocity
    , handle
    , render
    , reset
    , step
    ) where

import Base

data Player = Player
    { _position :: Point
    , _velocity :: Point
    , _rotation :: Point
    } deriving Show

makeLenses ''Player

render :: Player -> Picture
render p = renderPlayer <> renderSpeed <> renderAcceleration
 where
    renderPlayer = polygon $ getPoints p
    renderSpeed = color (makeColor 0 0 1 1)
        $ line [p ^. position, p ^. position + 0.2 .* p ^. velocity]
    renderAcceleration = color (makeColor 1 0 0 1)
        $ line [p ^. position, p ^. position + 0.2 .* getAcceleration p]

handle :: Event -> Player -> Player
handle (EventMotion (x, y)) p = p & rotation .~ unit (x, y)
handle _ p = p

step :: Float -> Player -> Player
step t = updatePosition . updateVelocity
  where
    updateVelocity p = p & velocity +~ t .* getAcceleration p
    updatePosition p = p & position +~ t .* p ^. velocity

create :: Point -> Player
create pos = Player pos 0 (1, 0)

reset :: Point -> Player -> Player
reset pos p = p & position .~ pos & velocity .~ 0

getPoints :: Player -> [Point]
getPoints p =
    [ (x + 20 * dx - 5 * dy, y + 20 * dy + 5 * dx)
    , (x + 20 * dx + 5 * dy, y + 20 * dy - 5 * dx)
    , (x - 20 * dx + 5 * dy, y - 20 * dy - 5 * dx)
    , (x - 20 * dx - 5 * dy, y - 20 * dy + 5 * dx)
    ]
  where
    (x, y) = p ^. position
    (dx, dy) = p ^. rotation

getPosition :: Player -> Point
getPosition = (^. position)

getVelocity :: Player -> Point
getVelocity = (^. velocity)

getAcceleration :: Player -> Point
getAcceleration p = (0, - gravity) - drag .* p ^. velocity - lift .* norm
  where
    aim = unit $ p ^. rotation
    norm = (\(x, y) -> (-y, x)) aim
    offset = uncurry (+) (norm * dir)
    dir = unit $ p ^. velocity
    lift = signum offset * getLift (mag $ p ^. velocity) (abs offset)

gravity :: Float
gravity = 200

drag :: Float
drag = 0.01

getLift :: Float -> Float -> Float
getLift v x = 0.002 * v ** 2 * x ** 0.3

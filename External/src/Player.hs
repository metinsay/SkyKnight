{-# LANGUAGE TemplateHaskell #-}

module Player
    ( Player
    , acceleration
    , create
    , handle
    , points
    , position
    , render
    , reset
    , step
    , velocity
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
    renderPlayer = polygon $ points p
    renderSpeed = color (makeColor 0 0 1 1)
        $ line [p ^. position, p ^. position + 0.2 .* p ^. velocity]
    renderAcceleration = color (makeColor 1 0 0 1)
        $ line [p ^. position, p ^. position + 0.2 .* acceleration p]

handle :: Event -> Player -> Player
handle (EventMotion (x, y)) p = p & rotation .~ unit (x, y)
handle _ p = p

step :: Float -> Player -> Player
step t = updatePosition . updateVelocity
  where
    updateVelocity p = p & velocity +~ t .* acceleration p
    updatePosition p = p & position +~ t .* p ^. velocity

create :: Point -> Player
create pos = Player pos 0 (1, 0)

reset :: Point -> Player -> Player
reset pos p = p & position .~ pos & velocity .~ 0

points :: Player -> [Point]
points p =
    [ (x + 20 * dx - 5 * dy, y + 20 * dy + 5 * dx)
    , (x + 20 * dx + 5 * dy, y + 20 * dy - 5 * dx)
    , (x - 20 * dx + 5 * dy, y - 20 * dy - 5 * dx)
    , (x - 20 * dx - 5 * dy, y - 20 * dy + 5 * dx)
    ]
  where
    (x, y) = p ^. position
    (dx, dy) = p ^. rotation

acceleration :: Player -> Point
acceleration p = (0, - 200) - 0.01 .* p ^. velocity - lift .* norm
  where
    aim = unit $ p ^. rotation
    norm = (\(x, y) -> (-y, x)) aim
    offset = uncurry (+) (norm * dir)
    dir = unit $ p ^. velocity
    lift = signum offset * 0.002 * mag (p ^. velocity) ** 2 * abs offset ** 0.3

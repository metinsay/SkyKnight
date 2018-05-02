{-# LANGUAGE TemplateHaskell #-}

module Player
    ( Player
    , acceleration
    , create
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
    , _image :: Picture
    } deriving Show

makeLenses ''Player

render :: Player -> Picture
render p = renderPlayer <> renderSpeed <> renderAcceleration
 where
    (px, py) = p ^. position
    (rx, ry) = p ^. rotation
    renderPlayer = translate px py $ rotate (atan2 (- ry) rx * 180 / pi) (p ^. image)
    renderSpeed = color (makeColor 0 0 1 1)
        $ line [p ^. position, p ^. position + 0.2 .* p ^. velocity]
    renderAcceleration = color (makeColor 1 0 0 1)
        $ line [p ^. position, p ^. position + 0.2 .* acceleration p]

step :: Float -> Point -> Player -> Player
step t c = updatePosition . updateVelocity . updateRotation
  where
    updateVelocity p = p & velocity +~ t .* acceleration p
    updatePosition p = p & position +~ t .* p ^. velocity
    updateRotation p = p & rotation .~ unit c

create :: Point -> IO Player
create pos = do
    Right (Just img) <- fmap fromDynamicImage <$> readImage "assets/player.png"
    pure $ Player pos 0 (1, 0) img

reset :: Point -> Player -> Player
reset pos p = p & position .~ pos & velocity .~ 0

points :: Player -> [Point]
points p =
    [ (x + 18 * dx - 6 * dy, y + 18 * dy + 6 * dx)
    , (x + 18 * dx + 18 * dy, y + 18 * dy - 18 * dx)
    , (x - 40 * dx + 18 * dy, y - 40 * dy - 18 * dx)
    , (x - 40 * dx - 6 * dy, y - 40 * dy + 6 * dx)
    ]
  where
    (x, y) = p ^. position
    (dx, dy) = p ^. rotation

acceleration :: Player -> Point
acceleration p = gravity - drag + lift
  where
    aim = unit $ p ^. rotation
    norm = (\(x, y) -> (-y, x)) aim
    offset = uncurry (+) (norm * dir)
    dir = unit $ p ^. velocity
    liftMag = signum offset * 0.002 * mag (p ^. velocity) ** 2 * abs offset ** 0.3 * (1 / physicsFactor)
    lift = -liftMag .* norm
    gravity = (0, -200 * physicsFactor)
    drag = 0.01 .* p ^. velocity

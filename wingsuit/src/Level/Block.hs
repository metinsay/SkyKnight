module Level.Block
    ( Block
    , inBlock
    , points
    , render
    ) where

import Base

type Block = (Point, Point)

render :: Block -> Picture
render b = polygon $ points b

points :: Block -> [Point]
points ((x1, y1), (x2, y2)) = [(x1, y1), (x1, y2), (x2, y2), (x2, y1)]

inBlock :: Point -> Block -> Bool
inBlock (x, y) ((x1, y1), (x2, y2)) = x > x1 && x < x2 && y > y1 && y < y2

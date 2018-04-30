module Block
    ( Block
    , center
    , inBlock
    ) where

import Base

type Block = (Point, Point)

center :: Block -> Point
center ((x1, y1), (x2, y2)) = ((x1 + x2) / 2, (y1 + y2) / 2)

inBlock :: Point -> Block -> Bool
inBlock (x, y) ((x1, y1), (x2, y2)) = x > x1 && x < x2 && y > y1 && y < y2

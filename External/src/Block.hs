module Block
    ( Block
    , render
    ) where

import Base

type Block = (Point, Point)

render :: Block -> Picture
render ((x1, y1), (x2, y2)) = polygon [(x1, y1), (x1, y2), (x2, y2), (x2, y1)]

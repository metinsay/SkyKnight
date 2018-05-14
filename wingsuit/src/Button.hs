module Button
    ( Button(Button)
    , handle
    , render
    ) where

import Base

data Button = Button String Point Point

render :: Button -> Picture
render (Button s (x1, y1) (x2, y2)) = renderBackground <> renderText
  where
    renderBackground = polygon [(x1, y1), (x1, y2), (x2, y2), (x2, y1)]
    renderText
        = color (makeColor 1 1 1 1)
        . translate (x1 * 0.95 + x2 * 0.05) (y1 * 0.75 + y2 * 0.25)
        . fuzz
        . join scale ((y2 - y1) / 200)
        $ text s

handle :: Event -> Button -> Bool
handle (EventKey (MouseButton LeftButton) Down _ (x, y)) (Button _ (x1, y1) (x2, y2))
    = x >= x1 && x <= x2 && y >= y1 && y <= y2
handle _ _ = False
